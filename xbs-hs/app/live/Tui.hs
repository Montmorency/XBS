{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | xbs-live's brick TUI head: a two-column terminal frontend on the shared
--   'App' state. Column 0 is a drill-down file explorer (load any sibling .bs at
--   runtime); column 1 stacks the controller (a live mirror of the render state +
--   key legend) over a braille preview of the molecule (so a pure-CLI user can
--   rotate and orient with no browser). Focus moves between columns with Tab/S-Tab
--   or M-h/M-l; the focused column consumes the arrow keys (tree navigation vs.
--   molecule rotation). The explorer writes 'LoadFile' / the controller writes
--   'Act' into the same 'cmdQ' the browser and terminal use, and a watcher mirrors
--   the shared render state (incl. browser-driven changes) back into the panes.
--
--   Structure follows utdemir's nix-tree (Brick.Widgets.List columns +
--   customMainWithDefaultVty + a BChan fed by a background thread). The preview is
--   a size-aware widget rasterized by "Braille".
module Tui (run) where

import Types
import XBS                                    (Picture)
import Braille                                (renderBraille, ColorMode(..), unpackRGB)

import qualified Brick                       as B
import qualified Brick.BChan                 as B
import qualified Brick.Widgets.Border        as B
import qualified Brick.Widgets.Border.Style  as BS
import qualified Brick.Widgets.List          as B
import           Brick.Util                  (on)
import           Lens.Micro                  ((^.))
import qualified Graphics.Vty                as Vty

import qualified Data.Vector                 as V
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

-- | TUI state. The render state proper lives in 'App'; here we keep the explorer
--   cursor, the focus, and a snapshot of the shared 'Status'/'Picture' (refreshed
--   on each 'Redraw') that the controller and preview panes draw from.
data TState = TState
  { tsCwd       :: FilePath
  , tsList      :: FileList
  , tsFocus     :: TName
  , tsStatus    :: Status
  , tsPicture   :: Picture
  , tsColorMode :: ColorMode
  }

-- | Custom brick event: the driver re-rendered (rotation, zoom, frame, load, or a
--   browser-driven command), so pull the latest 'Status'/'Picture' and repaint.
data TEvent = Redraw

-- | Launch the TUI. brick owns the main thread + vty; returning ends the process.
run :: App -> FilePath -> IO ()
run app startPath = do
  isDir <- doesDirectoryExist startPath
  startDir <- canonicalizePath (if isDir then startPath else takeDirectory startPath)
  entries  <- listDir startDir
  status0  <- readTVarIO app.statusTV
  pic0     <- readTVarIO app.pictureTV
  let ts0 = TState { tsCwd = startDir, tsList = mkList entries
                   , tsFocus = Explorer, tsStatus = status0, tsPicture = pic0
                   , tsColorMode = NoColor }
  chan <- B.newBChan 16
  _ <- forkIO (watch app chan)
  (_, vty) <- B.customMainWithDefaultVty (Just chan) (theApp app) ts0
  Vty.shutdown vty

-- | Wake the TUI on every render. STM @retry@ blocks on the driver's tick counter
--   so we repaint for *visual* changes (rotation, zoom, …) — not just 'Status'
--   ones (matches nix-tree's background feeder). The handler then pulls the latest
--   'Status'/'Picture'; @writeBChanNonBlocking@ self-throttles bursts (drops when
--   full) so fast browser drags don't flood the queue.
watch :: App -> B.BChan TEvent -> IO ()
watch app chan = readTVarIO app.tickTV >>= loop
  where
    loop old = do
      void (B.writeBChanNonBlocking chan Redraw)
      new <- atomically $ do
               v <- readTVar app.tickTV
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
theMap = B.attrMap Vty.defAttr
  [ (B.listSelectedFocusedAttr, Vty.black `on` Vty.cyan)
  , (B.listSelectedAttr,        Vty.defAttr `Vty.withStyle` Vty.dim)
  ]

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

draw :: TState -> [B.Widget TName]
draw ts =
  [ B.hBox
      [ B.hLimitPercent 45 $
          pane (ts.tsFocus == Explorer) (" explorer: " <> ts.tsCwd <> " ") (explorerW ts)
      , pane (ts.tsFocus == Controller) " controller " (rightPane ts)
      ]
  ]

-- | The controls/status (natural height) above the braille preview (fills the rest).
rightPane :: TState -> B.Widget TName
rightPane ts =
  B.vBox [ controllerW ts
         , B.hBorderWithLabel (B.str " preview ")
         , previewW ts.tsColorMode ts.tsPicture ]

-- | Size-aware braille preview: rasterizes the current 'Picture' to fill whatever
--   space brick allots this pane, reflowing on resize. The molecule reflows live
--   as the controller rotates/zooms it. In colour modes, each braille glyph gets
--   a vty foreground attribute via 'Vty.rgbColor' (24-bit true colour).
previewW :: ColorMode -> Picture -> B.Widget TName
previewW cm pic = B.Widget B.Greedy B.Greedy $ do
  ctx <- B.getContext
  let cols = ctx ^. B.availWidthL
      rows = ctx ^. B.availHeightL
      charRows = renderBraille cm cols rows pic
      rowImg row = Vty.horizCat [ charImg ch c | (ch, c) <- row ]
      charImg ch 0 = Vty.char Vty.defAttr ch
      charImg ch c = let (r, g, b) = unpackRGB c
                     in Vty.char (Vty.defAttr `Vty.withForeColor` Vty.rgbColor r g b) ch
  B.render (B.raw (Vty.vertCat (map rowImg charRows)))

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
controllerW ts = B.vBox $ map B.str
  [ "file:   " <> s.sFile
  , "atoms:  " <> show s.sNatoms
  , "frame:  " <> frameLine s
  , "zoom:   " <> showZoom s.sZoom
  , "render: persp " <> onoff s.sPersp <> " · line " <> onoff s.sBline
                     <> " · wire " <> onoff s.sWire <> " · labels " <> showLab s.sLabels
  , "color:  " <> showCM ts.tsColorMode
  , "focus:  " <> if null s.sFocus then "-" else show s.sFocus
  , ""
  , "── controls (this column) ──"
  , "  ← → ↑ ↓   rotate"
  , "  + / -     zoom"
  , "  p l w n r persp · line · wire · labels · reset"
  , "  c           braille color (off/gray/color)"
  , "  f / F     focus next atom / multi-toggle"
  , "  [ ] / j k  prev/next frame"
  , ""
  , "── tui ──"
  , "  Tab / S-Tab     switch column"
  , "  M-h / M-l       focus left / right"
  , "  q / Esc         quit"
  ]
  where
    s = ts.tsStatus
    onoff b = if b then "on" else "off"
    showCM NoColor   = "off"
    showCM GrayScale = "gray"
    showCM FullColor = "color"
    showLab LabelsOff     = "off"
    showLab LabelsIndex   = "idx"
    showLab LabelsSpecies = "species"
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
    B.AppEvent Redraw           -> do
      s <- liftIO (readTVarIO app.statusTV)
      p <- liftIO (readTVarIO app.pictureTV)
      put ts { tsStatus = s, tsPicture = p }
    B.VtyEvent (Vty.EvKey k mods) -> handleKey app ts k mods
    _                           -> pure ()

handleKey :: App -> TState -> Vty.Key -> [Vty.Modifier] -> B.EventM TName TState ()
handleKey app ts k mods
  | k `elem` [Vty.KChar 'q', Vty.KEsc]                      = B.halt
  | k == Vty.KChar '\t' || k == Vty.KBackTab               = put ts { tsFocus = other ts.tsFocus }
  | isMeta && k `elem` [Vty.KChar 'h', Vty.KChar 'l']      = put ts { tsFocus = other ts.tsFocus }
  | otherwise = case ts.tsFocus of
      Controller -> controllerKey app ts k mods
      Explorer   -> explorerKey app ts k
  where
    -- NB no M-[ / M-]: with iTerm "Esc+", Option-[ sends `ESC [` = the CSI escape
    -- introducer, which vty buffers then flushes as a stray KEsc (→ quit). M-h/M-l
    -- (and Tab/BackTab) avoid the escape-sequence space entirely.
    isMeta = Vty.MMeta `elem` mods
    other Explorer   = Controller
    other Controller = Explorer

-- | Controller pane keys: 'f' cycles single-atom focus forward (like clicking
--   each atom in turn); 'F' (shift) toggles the next atom into/out of the
--   multi-select stack (like shift-click). Everything else falls through to
--   'evToCmd' for rotation/zoom/frame/toggle commands.
controllerKey :: App -> TState -> Vty.Key -> [Vty.Modifier] -> B.EventM TName TState ()
controllerKey app ts k mods = case k of
    Vty.KChar 'f' | Vty.MShift `notElem` mods -> liftIO (cycleFocus app False)
    Vty.KChar 'F'                            -> liftIO (cycleFocus app True)
    Vty.KChar 'c'                            -> put ts { tsColorMode = nextCM ts.tsColorMode }
    _ -> liftIO (atomically (writeTChan app.cmdQ (Act (evToCmd (Vty.EvKey k mods)))))
  where nextCM FullColor = NoColor
        nextCM m         = succ m

-- | Advance the focus cursor to the next atom. Plain = replace (single select);
--   multi = toggle into/out of the stack (shift-click semantics). Wraps around;
--   one past the last atom clears the selection.
cycleFocus :: App -> Bool -> IO ()
cycleFocus app multi = atomically $ do
    natoms <- V.length <$> readTVar app.ballsTV
    if natoms == 0 then pure () else do
      cur <- readTVar app.focusTV
      let next = case cur of []    -> 0
                             (c:_) -> c + 1
          nf | next >= natoms = if multi then cur else []   -- wrap: clear (single) or stay (multi)
             | multi          = if next `elem` cur
                                  then filter (/= next) cur
                                  else next : cur
             | otherwise      = [next]
      writeTVar app.focusTV nf
      writeTChan app.cmdQ Refocus

explorerKey :: App -> TState -> Vty.Key -> B.EventM TName TState ()
explorerKey app ts = \case
    Vty.KUp        -> moveSel B.listMoveUp
    Vty.KChar 'k'  -> moveSel B.listMoveUp
    Vty.KDown      -> moveSel B.listMoveDown
    Vty.KChar 'j'  -> moveSel B.listMoveDown
    Vty.KEnter     -> activate
    Vty.KRight     -> activate
    Vty.KChar 'l'  -> activate
    Vty.KLeft      -> ascend
    Vty.KChar 'h'  -> ascend
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
