{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

-- | xbs-live: interactive ball-and-stick viewer.
--   Serves the current render over Server-Sent Events on :9090 (open the page in
--   a browser); drives the view from the terminal (vty). The input loop is a
--   delimited-continuation coroutine (CC-delcont): each keypress suspends at a
--   prompt, the rest-of-the-render-loop is the captured continuation, and we
--   resume with the key. Config is the loop accumulator (no shared Config TVar);
--   the only shared cell is the latest-SVG TVar the SSE handler streams from.

module Main (main) where

import XBS
import ParseXBS (loadBs, parseMv)

import qualified Data.Vector            as V
import qualified Data.Text              as T
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Encoding     as TE
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy   as LBS

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.STM
import           Control.Exception       (finally)
import           Control.Monad           (forever, join)
import           Control.Monad.IO.Class  (liftIO)
import           System.Environment      (getArgs)
import           System.FilePath         (replaceExtension)
import           System.Directory        (doesFileExist)
import           Text.Read               (readMaybe)

import           Control.Monad.CC.CCRef  -- multi-prompt delimited control (Oleg's reference impl)

import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.HTTP.Types       (status200, status404, hContentType, hCacheControl)

import qualified Graphics.Vty            as Vty
import qualified Graphics.Vty.CrossPlatform as VC  -- vty>=6 moved mkVty here

port :: Int
port = 9090

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--dump":path:_) -> dumpHtml path
    (path:_)          -> runLive path
    _                 -> putStrLn "usage: xbs-live [--dump] <file.bs>"

-- | Print the markup the server emits (page shell, a sample data panel, one live
--   SVG frame) without starting vty/warp. For eyeballing the htmx wiring offline.
dumpHtml :: FilePath -> IO ()
dumpHtml path = do
  src <- readFile path
  let (cfg0, balls, bondMap) = loadBs src
      bs    = V.fromList (stick bondMap balls)
      focus = take 2 (map (.idx) balls)        -- sample: first two atoms focused
  putStrLn "=== page shell ==="
  LBS.putStr pageHtml >> putStrLn ""
  putStrLn "\n=== focusPanel (atoms 0,1) ==="
  TIO.putStrLn (focusPanel (V.fromList balls) focus)
  putStrLn "\n=== renderConfigSvg frame0 (first 1100 chars) ==="
  TIO.putStrLn (T.take 1100 (renderConfigSvg focus cfg0 bs))

-- | A loaded movie: per frame, the atoms' positions (atom order matches the .bs).
--   Empty when there's no sibling @.mv@ (static single-frame view).
type Movie = V.Vector (V.Vector Vec3)

-- | One unit of input to the render loop. Both the terminal (vty keypresses,
--   pumped by a thread) and the browser feed the same queue, so every input
--   mutates the one 'Config' and broadcasts to all viewers — terminal and every
--   browser stay in sync automatically. The delimcc listener selects over it.
data Input = Key Vty.Event          -- ^ a terminal keypress
           | Act Cmd                -- ^ a browser view command (keydown / wheel → /cmd)
           | RotDelta Double Double  -- ^ a browser pointer-drag (dx,dy px → versor rotate)
           | Refocus                -- ^ the browser changed the focus stack; just re-render

-- | Pointer-drag rotation sensitivity (radians per pixel).
dragSens :: Double
dragSens = 0.01

-- | Shared state + static assets handed to the warp app and the driver. The
--   render loop owns 'Config' (its accumulator); these are the cross-thread cells.
data App = App
  { tv      :: TVar Text             -- ^ latest rendered SVG (SSE broadcast cell)
  , focusTV :: TVar [Int]            -- ^ the focus stack (selected atom indices), newest first
  , ballsTV :: TVar (V.Vector Ball)  -- ^ current-frame balls (for the data panel)
  , cmdQ    :: TChan Input           -- ^ terminal + browser inputs the listener reads
  , htmxJs  :: LBS.ByteString        -- ^ vendored htmx.min.js (for the atom-click hx-get)
  }

runLive :: FilePath -> IO ()
runLive path = do

  -- | Switch readFile to open a streaming connection
  src <- readFile path

  let (cfg0, balls, bondMap) = loadBs src
      home  = cfg0.tmat                          -- for the reset key

  movie  <- loadMovie path (length balls)        -- sibling <file>.mv, if present
  htmxJs <- readAsset "static/htmx.min.js"       -- served at /htmx.min.js

  tvar   <- newTVarIO ("" :: Text)
  focusV <- newTVarIO ([] :: [Int])
  ballsV <- newTVarIO (V.fromList balls)
  cmdq   <- newTChanIO
  let app = App tvar focusV ballsV cmdq htmxJs

  _  <- forkIO (run port (sseApp app))

  vty <- VC.mkVty Vty.defaultConfig
  -- pump terminal events into the shared queue (browser /focus writes Refocus there too)
  _  <- forkIO (forever (Vty.nextEvent vty >>= atomically . writeTChan cmdq . Key))

  Vty.update vty $ Vty.picForImage $ Vty.string Vty.defAttr (xbsDocString port (V.length movie))

  driver app home bondMap balls movie cfg0 `finally` Vty.shutdown vty

-- | Read a vendored static asset; warn (and serve nothing) if absent so the app
--   still starts — the SVG renders, only browser interactivity is lost.
readAsset :: FilePath -> IO LBS.ByteString
readAsset p = do
  ok <- doesFileExist p
  if ok then LBS.readFile p
        else do putStrLn ("warning: missing asset " <> p <> " — browser interactivity disabled")
                pure ""

-- | Load the sibling @<file>.mv@ movie (replace the extension). Returns 'V.empty'
--   when absent. Frames whose atom count doesn't match the .bs are dropped (guards
--   against a truncated trailing frame).
--
--   EAGER for now: the whole movie is parsed and held resident (the comprehension
--   forces every frame). Fine for ring.mv (~122 KB); revisit for large trajectories.
--   TODO(streaming): swap this whole-file load for on-demand frame fetch. Options:
--     * pipes/conduit — stream frames off disk, advance pulls the next element;
--     * a delimcc zipper — the captured render-loop continuation (listenForChar's
--       @p@) is already the suspension seam, so a frame could be yielded into the
--       loop per keypress instead of indexing a resident Vector. See memory
--       [[xbs-mv-streaming-followup]].
loadMovie :: FilePath -> Int -> IO Movie
loadMovie path natoms = do
  let mvPath = replaceExtension path "mv"
  exists <- doesFileExist mvPath
  if not exists
    then pure V.empty
    else do
      txt <- readFile mvPath
      pure $ V.fromList [ V.fromList f | f <- parseMv natoms txt, length f == natoms ]

-- | The renderable (balls + freshly built bonds) for a given frame. With no movie
--   the topology is the static .bs; otherwise each ball takes its frame position
--   and bonds are rebuilt (distance rules re-evaluated as atoms move).
sceneAt :: BondMap -> [Ball] -> Movie -> Int -> V.Vector (Ball, [Stick])
sceneAt bondMap balls movie f
  | V.null movie = V.fromList (stick bondMap balls)
  | otherwise    =
      let coords = movie V.! max 0 (min (V.length movie - 1) f)
          balls' = zipWith (\b p -> b { pos = p }) balls (V.toList coords)
      in V.fromList (stick bondMap balls')


xbsDocString :: Int -> Int -> String
xbsDocString port nframes =
  "xbs-live -> http://localhost:" <> show port
  <> "   (arrows ',/ rotate, <> spin, +/- zoom, p persp, l line, w wire, r reset, q quit"
  <> framesHint <> ")"
  where framesHint = if nframes > 1 then ", [ ] j k frames(" <> show nframes <> ")" else ""


-- | Delimited-continuation driver: render+publish → suspend for an 'Input' →
--   apply. Inputs arrive from both the terminal and the browser via one queue.
--   A 'Key' may rotate/zoom/step frames (rebuilds the scene only when the frame
--   index changes); a 'Refocus' just re-renders so browser-selected atoms light
--   up in the SVG. @nframes@ drives modular frame wraparound in 'applyCmd'.
driver :: App -> Mat3 -> BondMap -> [Ball] -> Movie -> Config -> IO ()
driver app home bondMap balls movie cfg0 = runCC $ do
    p <- newPrompt
    let nframes    = max 1 (V.length movie)
        sceneFor f = sceneAt bondMap balls movie f
        -- read the focus stack, stash the frame's balls for the data panel, render
        publish cfg bs = liftIO $ do
          focus <- readTVarIO app.focusTV
          atomically $ do
            writeTVar app.ballsTV (V.map fst bs)
            writeTVar app.tv (renderConfigSvg focus cfg bs)
        -- apply a discrete Cmd (from terminal or browser); rebuild scene on frame change
        runCmd cmd cfg bs = case cmd of
          Quit -> pure ()
          _    -> let cfg' = applyCmd home nframes cmd cfg
                      bs'  = if cfg'.frame == cfg.frame then bs else sceneFor cfg'.frame
                  in go cfg' bs'
        go cfg bs = do
          publish cfg bs
          inp <- listenForInput p app.cmdQ
          case inp of
            Refocus        -> go cfg bs                   -- selection changed: re-render only
            Key ev         -> runCmd (evToCmd ev) cfg bs
            Act cmd        -> runCmd cmd cfg bs
            RotDelta dx dy -> go (cfg { tmat = rotmat 1 (dragSens*dx)
                                                 (rotmat 2 (dragSens*dy) cfg.tmat) }) bs
    pushPrompt p (go cfg0 (sceneFor cfg0.frame))

-- | The suspension point: capture the render-loop continuation at @p@, block for
--   the next 'Input' (terminal key or browser refocus), resume. The captured
--   continuation is the seam for future focus -> calc/DB interleaving.
listenForInput :: Prompt IO () -> TChan Input -> CC IO Input
listenForInput p q = takeSubCont p $ \sk ->
    pushPrompt p $ do
        inp <- liftIO (atomically (readTChan q))
        pushSubCont sk (pure inp)

evToCmd :: Vty.Event -> Cmd
evToCmd (Vty.EvKey k _) = case k of
    Vty.KLeft      -> RotL
    Vty.KRight     -> RotR
    Vty.KUp        -> RotU
    Vty.KDown      -> RotD
    Vty.KChar '\'' -> RotU
    Vty.KChar '/'  -> RotD
    Vty.KChar '<'  -> RotCCW
    Vty.KChar '>'  -> RotCW
    Vty.KChar 'p'  -> TogglePersp
    Vty.KChar 'l'  -> ToggleLine
    Vty.KChar 'w'  -> ToggleWire
    Vty.KChar '+'  -> ZoomIn
    Vty.KChar '='  -> ZoomIn            -- unshifted '+' key
    Vty.KChar '-'  -> ZoomOut
    Vty.KChar '_'  -> ZoomOut           -- shifted '-' key
    Vty.KChar 'r'  -> ResetView
    Vty.KChar '['  -> FramePrev
    Vty.KChar ']'  -> FrameNext
    Vty.KChar 'j'  -> FrameFirst
    Vty.KChar 'k'  -> FrameLast
    Vty.KChar 'q'  -> Quit
    Vty.KEsc       -> Quit
    _              -> NoOp
evToCmd _ = NoOp

------------------------------------------------------------------------
-- SSE web server (warp + wai)
------------------------------------------------------------------------

sseApp :: App -> Application
sseApp app req respond = case pathInfo req of
    []              -> respond $ responseLBS status200
                         [(hContentType, "text/html; charset=utf-8")] pageHtml
    ["events"]      -> respond $ responseStream status200
                         [(hContentType, "text/event-stream"), (hCacheControl, "no-cache")]
                         (sseStream app.tv)
    ["focus"]       -> focusHandler app req respond
    ["cmd"]         -> cmdHandler app req respond
    ["htmx.min.js"] -> respond $ responseLBS status200 [(hContentType, "text/javascript")] app.htmxJs
    _               -> respond $ responseLBS status404 [] "not found"

-- | Browser view command (@GET /cmd?c=…@): keystrokes/wheel map to a discrete
--   'Cmd' (@c=rotl|zoomin|wire|framenext|…@), and pointer-drag sends
--   @c=rot&dx=..&dy=..@ → 'RotDelta'. Writes into the shared queue, so the browser
--   drives the exact same Config the terminal does. (Quit is intentionally NOT
--   reachable from the browser — one viewer shouldn't kill a shared session.)
cmdHandler :: App -> Application
cmdHandler app req respond = do
  case TE.decodeUtf8 <$> join (lookup "c" q) of
    Just "rot" -> enqueue (RotDelta (num "dx") (num "dy"))
    Just t     -> maybe (pure ()) (enqueue . Act) (tokenToCmd t)
    Nothing    -> pure ()
  respond (responseLBS status200 [] "")
  where
    q          = queryString req
    enqueue    = atomically . writeTChan app.cmdQ
    num k      = maybe 0 id (join (lookup k q) >>= readMaybe . T.unpack . TE.decodeUtf8)

-- | Browser command token → view 'Cmd' (mirrors the terminal keys in 'evToCmd').
tokenToCmd :: Text -> Maybe Cmd
tokenToCmd t = case t of
  "rotl"      -> Just RotL    ; "rotr"      -> Just RotR
  "rotu"      -> Just RotU    ; "rotd"      -> Just RotD
  "rotccw"    -> Just RotCCW  ; "rotcw"     -> Just RotCW
  "zoomin"    -> Just ZoomIn  ; "zoomout"   -> Just ZoomOut
  "persp"     -> Just TogglePersp
  "line"      -> Just ToggleLine
  "wire"      -> Just ToggleWire
  "reset"     -> Just ResetView
  "frameprev" -> Just FramePrev   ; "framenext"  -> Just FrameNext
  "framefirst"-> Just FrameFirst  ; "framelast"  -> Just FrameLast
  _           -> Nothing

-- | Atom selection (htmx @GET /focus?atom=I&multi=M@). Updates the focus stack —
--   plain click replaces, shift-click (multi=1) adds or toggles — wakes the render
--   loop ('Refocus', so the SVG re-highlights), and returns the data-panel fragment
--   htmx swaps into @#data@.
focusHandler :: App -> Application
focusHandler app req respond =
  case join (lookup "atom" q) >>= readMaybe . T.unpack . TE.decodeUtf8 of
    Nothing   -> respond (responseLBS status200 htmlHdr "")
    Just atom -> do
      newFocus <- atomically $ do
        cur <- readTVar app.focusTV
        let nf = updateFocus multi atom cur
        writeTVar app.focusTV nf
        writeTChan app.cmdQ Refocus
        pure nf
      balls <- readTVarIO app.ballsTV
      respond $ responseLBS status200 htmlHdr
        (LBS.fromStrict (TE.encodeUtf8 (focusPanel balls newFocus)))
  where
    q       = queryString req
    multi   = maybe False (/= "0") (join (lookup "multi" q))
    htmlHdr = [(hContentType, "text/html; charset=utf-8")]

-- | Focus-stack semantics: plain click focuses just that atom (replace); shift
--   adds it, or removes it if already present (toggle). Newest is at the front.
updateFocus :: Bool -> Int -> [Int] -> [Int]
updateFocus multi atom cur
  | multi     = if atom `elem` cur then filter (/= atom) cur else atom : cur
  | otherwise = [atom]

-- | Stream the latest SVG whenever it changes. STM @retry@ on the shared TVar is
--   the broadcast: every connected client wakes and emits the new frame. The
--   initial read (old = "") sends the current frame to late-joining browsers.
sseStream :: TVar Text -> StreamingBody
sseStream tv write flush = go ""
  where
    go old = do
      new <- atomically $ do
               v <- readTVar tv
               if v == old then retry else return v
      write $ BB.byteString "data: "
           <> BB.byteString (TE.encodeUtf8 (oneLine new))
           <> BB.byteString "\n\n"
      flush
      go new
    oneLine = T.map (\c -> if c == '\n' then ' ' else c)   -- SSE data is single-line

-- | Two-pane shell: | #data (focus/inspection) | #view (SVG) |. The SVG streams
--   over a plain EventSource; after each frame we call @htmx.process(view)@ so the
--   freshly-streamed atom circles get their @hx-get@ click bindings wired (a raw
--   innerHTML assignment alone wouldn't activate them). Atom selection itself is
--   declarative htmx (each circle's hx-get → /focus → swaps #data); only the
--   stream-receive is hand-rolled (htmx's SSE extension is API-incompatible with
--   core 2.0.8 — calls a removed api.selectAndSwap).
pageHtml :: LBS.ByteString
pageHtml = LBS.fromStrict $ TE.encodeUtf8 $ T.concat
    [ "<!doctype html><html><head><meta charset=utf-8><title>xbs-live</title>"
    , "<style>"
    , "html,body{margin:0;height:100%;font-family:sans-serif}"
    , "body{display:flex}"
    , "#data{width:300px;flex:0 0 300px;overflow:auto;padding:12px;"
    ,       "border-right:1px solid #eee;box-sizing:border-box}"
    , "#view{flex:1;display:flex;align-items:center;justify-content:center;background:#fff}"
    , "#view svg{max-width:100%;max-height:100vh}"
    , "</style>"
    , "<script src=\"/htmx.min.js\"></script>"
    , "</head><body>"
    , "<div id=\"data\">click an atom · shift-click to add</div>"
    , "<div id=\"view\"></div>"
    , "<script>"
    , "var view=document.getElementById('view');"
    -- stream the SVG; htmx.process wires the new circles' hx-get bindings
    , "new EventSource('/events').onmessage=function(e){view.innerHTML=e.data;htmx.process(view);};"
    , "function send(q){fetch('/cmd?'+q);}"
    -- keystroke parity with the terminal (same keys drive the shared Config)
    , "var km={ArrowLeft:'rotl',ArrowRight:'rotr',ArrowUp:'rotu',ArrowDown:'rotd',"
    ,   "'<':'rotccw','>':'rotcw',p:'persp',l:'line',w:'wire',r:'reset',"
    ,   "'[':'frameprev',']':'framenext',j:'framefirst',k:'framelast',"
    ,   "'+':'zoomin','=':'zoomin','-':'zoomout'};"
    , "addEventListener('keydown',function(e){var c=km[e.key];if(c){send('c='+c);e.preventDefault();}});"
    -- trackpad / wheel → zoom
    , "view.addEventListener('wheel',function(e){send('c='+(e.deltaY<0?'zoomin':'zoomout'));e.preventDefault();},{passive:false});"
    -- pointer-drag → rotate (deltas accumulated, flushed once per animation frame)
    , "var down=false,ax=0,ay=0;"
    , "view.addEventListener('pointerdown',function(){down=true;});"
    , "addEventListener('pointerup',function(){down=false;});"
    , "addEventListener('pointermove',function(e){if(down){ax+=e.movementX;ay+=e.movementY;}});"
    , "function tick(){if(ax||ay){send('c=rot&dx='+ax+'&dy='+ay);ax=0;ay=0;}requestAnimationFrame(tick);}tick();"
    , "</script>"
    , "</body></html>" ]
