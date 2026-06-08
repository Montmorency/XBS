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
import ParseXBS (loadBs)

import qualified Data.Vector            as V
import qualified Data.Text              as T
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as TE
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy   as LBS

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.STM
import           Control.Exception       (finally)
import           Control.Monad.IO.Class  (liftIO)
import           System.Environment      (getArgs)

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
    (path:_) -> runLive path
    _        -> putStrLn "usage: xbs-live <file.bs>"

runLive :: FilePath -> IO ()
runLive path = do

  -- | Switch readFile to open a streaming connection
  src <- readFile path
  
  let (cfg0, balls, bondMap) = loadBs src
      bs    = V.fromList (stick bondMap balls)   -- topology built once
      home  = cfg0.tmat                          -- for the reset key
      
  tv <- newTVarIO ("" :: Text)                   -- latest rendered SVG (the one shared cell)
  
  _  <- forkIO (run port (sseApp tv))
  
  vty <- VC.mkVty Vty.defaultConfig
  
  Vty.update vty $ Vty.picForImage $ Vty.string Vty.defAttr (xbsDocString port)
      
  driver tv vty home bs cfg0 `finally` Vty.shutdown vty



xbsDocString port = "xbs-live -> http://localhost:" <> show port <> "   (arrows, ', / : rotate, < > spin, p persp, l line, w wire, r reset, q quit)"


-- | Delimited-continuation driver: render → publish → suspend for a key → apply:
driver :: TVar Text -> Vty.Vty -> Mat3 -> V.Vector (Ball, [Stick]) -> Config -> IO ()
driver tv vty home bs cfg0 = runCC $ do
    p <- newPrompt
    let go cfg = do
          liftIO $ atomically $ writeTVar tv (renderConfigSvg cfg bs)
          ev <- listenForChar p vty
          case evToCmd ev of
            Quit -> pure ()
            cmd  -> go (applyCmd home cmd cfg)
    pushPrompt p (go cfg0)

-- | The suspension point: capture the render-loop continuation at @p@, read a
--   key, resume. Phase 1 resumes immediately; the captured continuation is the
--   seam for future focus -> calc/DB interleaving.
listenForChar :: Prompt IO () -> Vty.Vty -> CC IO Vty.Event
listenForChar p vty = takeSubCont p $ \sk ->
    pushPrompt p $ do
        ev <- liftIO (Vty.nextEvent vty)
        pushSubCont sk (pure ev)

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

sseApp :: TVar Text -> Application
sseApp tv req respond = case pathInfo req of
    []         -> respond $ responseLBS status200
                    [(hContentType, "text/html; charset=utf-8")] pageHtml
    ["events"] -> respond $ responseStream status200
                    [(hContentType, "text/event-stream"), (hCacheControl, "no-cache")]
                    (sseStream tv)
    _          -> respond $ responseLBS status404 [] "not found"

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

pageHtml :: LBS.ByteString
pageHtml = LBS.fromStrict $ TE.encodeUtf8 $ T.concat
    [ "<!doctype html><html><head><meta charset=utf-8><title>xbs-live</title>"
    , "<style>html,body{margin:0;height:100%}"
    , "body{display:flex;align-items:center;justify-content:center;background:#fff}</style>"
    , "</head><body><div id=view></div>"
    , "<script>new EventSource('/events').onmessage="
    , "function(e){document.getElementById('view').innerHTML=e.data}</script>"
    , "</body></html>" ]
