{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | xbs-live's brick TUI head: a two-column terminal frontend on the shared
--   'App' state. Column 0 is a drill-down file explorer (load any sibling .bs at
--   runtime); column 1 is the controller — a live mirror of the render state plus
--   the key legend. Focus moves between columns with Tab or M-[ / M-]; the focused
--   column consumes the arrow keys (tree navigation vs. molecule rotation). The
--   explorer writes 'LoadFile' / the controller writes 'Act' into the same
--   'cmdQ' the browser and terminal use, and a watcher mirrors 'statusTV' changes
--   (incl. browser-driven ones) back into the controller pane.
--
--   Structure follows utdemir's nix-tree (Brick.Widgets.List columns +
--   customMainWithDefaultVty + a BChan fed by a background thread).
module Tui (run) where

import Types

import qualified Brick                       as B
import qualified Brick.BChan                 as B
import qualified Brick.Widgets.Border        as B
import qualified Brick.Widgets.Border.Style  as BS
import qualified Brick.Widgets.List          as B
import           Brick.Util                  (on)
import qualified Graphics.Vty                as V

import qualified Data.Sequence               as Seq
import           Data.Sequence               (Seq)
import           Data.List                   (sortOn)
import           Control.Monad               (void, forM)
import           Control.Monad.State         (get, put)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Concurrent          (forkIO)
import           Control.Concurrent.STM
import           Control.Exception           (try, SomeException)
import           System.Directory            ( listDirectory, doesDirectoryExist
                                             , canonicalizePath )
import           System.FilePath             ((</>), takeDirectory, takeExtension)

-- | Resource names: also identifies the focused column.
data TName = Explorer | Controller
  deriving (Eq, Ord, Show)

-- | One explorer row. @eName@ is the display/leaf name (or "..").
data Entry = Entry { eName :: String, eIsDir :: Bool }

type FileList = B.GenericList TName Seq Entry

-- | TUI state. The render state proper lives in 'App'/'statusTV'; here we keep
--   only what the terminal frontend owns: the explorer cursor and the focus.
data TState = TState
  { tsCwd    :: FilePath
  , tsList   :: FileList
  , tsFocus  :: TName
  , tsStatus :: Status
  }

-- | Custom brick event: the shared 'statusTV' changed (driver re-rendered, or the
--   browser drove a command), so refresh the controller pane.
newtype TEvent = StatusChanged Status

-- | Launch the TUI. brick owns the main thread + vty; returning ends the process.
run :: App -> FilePath -> IO ()
run app startPath = do
  startDir <- canonicalizePath (takeDirectory startPath)
  entries  <- listDir startDir
  status0  <- readTVarIO app.statusTV
  let ts0 = TState { tsCwd = startDir, tsList = mkList entries
                   , tsFocus = Explorer, tsStatus = status0 }
  chan <- B.newBChan 16
  _ <- forkIO (watch app chan)
  (_, vty) <- B.customMainWithDefaultVty (Just chan) (theApp app) ts0
  V.shutdown vty

-- | Mirror every 'statusTV' change into the TUI via the BChan. STM @retry@ blocks
--   until the value actually differs (matches nix-tree's background feeder).
watch :: App -> B.BChan TEvent -> IO ()
watch app chan = readTVarIO app.statusTV >>= loop
  where
    loop old = do
      void (B.writeBChanNonBlocking chan (StatusChanged old))
      new <- atomically $ do
               v <- readTVar app.statusTV
               if v == old then retry else pure v
      loop new

theApp :: App -> B.App TState TEvent TName
theApp app = B.App
  { B.appDraw         = draw
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = handleEvent app
  , B.appStartEvent   = pure ()
  , B.appAttrMap      = const theMap
  }

theMap :: B.AttrMap
theMap = B.attrMap V.defAttr
  [ (B.listSelectedFocusedAttr, V.black `on` V.cyan)
  , (B.listSelectedAttr,        V.defAttr `V.withStyle` V.dim)
  ]

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

draw :: TState -> [B.Widget TName]
draw ts =
  [ B.hBox
      [ B.hLimitPercent 45 $
          pane (ts.tsFocus == Explorer) (" explorer: " <> ts.tsCwd <> " ") (explorerW ts)
      , pane (ts.tsFocus == Controller) " controller " (controllerW ts)
      ]
  ]

-- | A bordered column; the focused one gets a bold border so it's obvious which
--   column the arrow keys are driving.
pane :: Bool -> String -> B.Widget TName -> B.Widget TName
pane focused label body =
  B.withBorderStyle (if focused then BS.unicodeBold else BS.unicodeRounded)
    (B.borderWithLabel (B.str label) body)

explorerW :: TState -> B.Widget TName
explorerW ts = B.renderList renderEntry (ts.tsFocus == Explorer) ts.tsList
  where
    renderEntry _ e = B.str ((if e.eIsDir then "▸ " else "  ") <> e.eName)

controllerW :: TState -> B.Widget TName
controllerW ts = B.padBottom B.Max $ B.vBox $ map B.str
  [ "file:   " <> s.sFile
  , "atoms:  " <> show s.sNatoms
  , "frame:  " <> frameLine s
  , "zoom:   " <> showZoom s.sZoom
  , "render: persp " <> onoff s.sPersp <> " · line " <> onoff s.sBline
                     <> " · wire " <> onoff s.sWire
  , "focus:  " <> if null s.sFocus then "-" else show s.sFocus
  , ""
  , "── controls (this column) ──"
  , "  ← → ↑ ↓   rotate"
  , "  + / -     zoom"
  , "  p l w r   persp · line · wire · reset"
  , "  [ ] j k   frames"
  , ""
  , "── tui ──"
  , "  Tab / S-Tab     switch column"
  , "  M-h / M-l       focus left / right"
  , "  q / Esc         quit"
  ]
  where
    s = ts.tsStatus
    onoff b = if b then "on" else "off"
    frameLine st | st.sNframes > 1 = show (st.sFrame + 1) <> "/" <> show st.sNframes
                 | otherwise       = "(static)"
    showZoom z = show (fromIntegral (round (z * 100) :: Int) / 100 :: Double)

------------------------------------------------------------------------
-- Events
------------------------------------------------------------------------

handleEvent :: App -> B.BrickEvent TName TEvent -> B.EventM TName TState ()
handleEvent app ev = do
  ts <- get
  case ev of
    B.AppEvent (StatusChanged s) -> put ts { tsStatus = s }
    B.VtyEvent (V.EvKey k mods)  -> handleKey app ts k mods
    _                            -> pure ()

handleKey :: App -> TState -> V.Key -> [V.Modifier] -> B.EventM TName TState ()
handleKey app ts k mods
  | k `elem` [V.KChar 'q', V.KEsc]                      = B.halt
  | k == V.KChar '\t' || k == V.KBackTab               = put ts { tsFocus = other ts.tsFocus }
  | isMeta && k `elem` [V.KChar 'h', V.KChar 'l']      = put ts { tsFocus = other ts.tsFocus }
  | otherwise = case ts.tsFocus of
      Controller -> liftIO (atomically (writeTChan app.cmdQ (Act (evToCmd (V.EvKey k mods)))))
      Explorer   -> explorerKey app ts k
  where
    -- NB no M-[ / M-]: with iTerm "Esc+", Option-[ sends `ESC [` = the CSI escape
    -- introducer, which vty buffers then flushes as a stray KEsc (→ quit). M-h/M-l
    -- (and Tab/BackTab) avoid the escape-sequence space entirely.
    isMeta = V.MMeta `elem` mods
    other Explorer   = Controller
    other Controller = Explorer

explorerKey :: App -> TState -> V.Key -> B.EventM TName TState ()
explorerKey app ts = \case
    V.KUp        -> moveSel B.listMoveUp
    V.KChar 'k'  -> moveSel B.listMoveUp
    V.KDown      -> moveSel B.listMoveDown
    V.KChar 'j'  -> moveSel B.listMoveDown
    V.KEnter     -> activate
    V.KRight     -> activate
    V.KChar 'l'  -> activate
    V.KLeft      -> ascend
    V.KChar 'h'  -> ascend
    _            -> pure ()
  where
    moveSel f = put ts { tsList = f ts.tsList }
    ascend    = descendInto ts (takeDirectory ts.tsCwd)
    activate  = case B.listSelectedElement ts.tsList of
      Nothing -> pure ()
      Just (_, e)
        | e.eName == ".." -> descendInto ts (takeDirectory ts.tsCwd)
        | e.eIsDir        -> descendInto ts (ts.tsCwd </> e.eName)
        | otherwise       -> liftIO (atomically (writeTChan app.cmdQ
                                       (LoadFile (ts.tsCwd </> e.eName))))

-- | Relist @dir@ and move the explorer there; a listing error (permissions, gone)
--   leaves the current view untouched.
descendInto :: TState -> FilePath -> B.EventM TName TState ()
descendInto ts dir = do
  r <- liftIO (try (listDir dir) :: IO (Either SomeException (Seq Entry)))
  case r of
    Left _        -> pure ()
    Right entries -> put ts { tsCwd = dir, tsList = mkList entries }

------------------------------------------------------------------------
-- Filesystem
------------------------------------------------------------------------

mkList :: Seq Entry -> FileList
mkList entries = B.list Explorer entries 1

-- | Directory contents for the explorer: a leading "..", then subdirectories,
--   then *.bs files — each group alphabetical.
listDir :: FilePath -> IO (Seq Entry)
listDir dir = do
  names      <- listDirectory dir
  annotated  <- forM names $ \n -> do
                  isd <- doesDirectoryExist (dir </> n)
                  pure (n, isd)
  let dirs  = [ Entry n True  | (n, True)  <- annotated ]
      files = [ Entry n False | (n, False) <- annotated, takeExtension n == ".bs" ]
  pure $ Seq.fromList
       $ Entry ".." True : sortOn (.eName) dirs ++ sortOn (.eName) files
