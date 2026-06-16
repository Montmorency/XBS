{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Shared state and input types for xbs-live's three heads (browser, terminal
--   driver, brick TUI). Lives in its own module so both 'Main' (server + driver)
--   and 'Tui' (the brick frontend) can import it without a cycle.
module Types
  ( Input(..)
  , App(..)
  , Status(..)
  , Labels(..)
  , evToCmd
  ) where

import XBS (Cmd(..), Labels(..), Ball, Picture)

import           Data.Text               (Text)
import qualified Data.Vector             as V
import           Control.Concurrent.STM  (TVar, TChan)
import qualified Data.ByteString.Lazy    as LBS
import qualified Graphics.Vty            as Vty

-- | One unit of input to the render loop. The terminal driver, the browser, and
--   the brick TUI all feed the same queue, so every input mutates the one
--   'Config' and broadcasts to all viewers. The delimcc listener selects over it.
data Input = Key Vty.Event          -- ^ a terminal keypress
           | Act Cmd                -- ^ a view command (browser keydown/wheel, or TUI controller)
           | RotDelta Double Double  -- ^ a browser pointer-drag (dx,dy px → versor rotate)
           | Refocus                -- ^ the browser changed the focus stack; just re-render
           | LoadFile FilePath      -- ^ the TUI picked a new .bs file; swap the whole scene

-- | Shared state + static assets handed to the warp app, the driver, and the TUI.
--   The render loop owns 'Config' (its accumulator); these are the cross-thread
--   cells. 'statusTV' is the controller-pane projection of the current Config.
data App = App
  { tv       :: TVar Text             -- ^ latest rendered SVG (SSE broadcast cell)
  , focusTV  :: TVar [Int]            -- ^ the focus stack (selected atom indices), newest first
  , ballsTV  :: TVar (V.Vector Ball)  -- ^ current-frame balls (for the data panel)
  , cmdQ     :: TChan Input           -- ^ terminal + browser + TUI inputs the listener reads
  , htmxJs   :: LBS.ByteString        -- ^ vendored htmx.min.js (for the atom-click hx-get)
  , statusTV :: TVar Status           -- ^ live view state, mirrored into the TUI controller pane
  , pictureTV:: TVar Picture          -- ^ current scene as the backend-neutral IR (TUI rasterizes it)
  , tickTV   :: TVar Int              -- ^ render counter, bumped every publish; wakes the TUI watcher
  }

-- | A snapshot of the render loop's 'Config' (+ scene facts) for the TUI
--   controller pane. Published by the driver on every frame so the TUI reflects
--   browser- and terminal-driven changes too.
data Status = Status
  { sFile    :: FilePath   -- ^ basename of the loaded .bs
  , sNatoms  :: Int
  , sFrame   :: Int
  , sNframes :: Int
  , sZoom    :: Double
  , sPersp   :: Bool
  , sBline   :: Bool
  , sWire    :: Bool
  , sLabels  :: Labels
  , sFocus   :: [Int]
  }
  deriving (Eq)

-- | Terminal/TUI keypress → view 'Cmd'. Shared by the old terminal pump and the
--   brick controller pane (brick's @VtyEvent@ carries the same 'Vty.Event').
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
    Vty.KChar 'n'  -> ToggleLabels
    Vty.KChar '+'  -> ZoomIn
    Vty.KChar '='  -> ZoomIn            -- unshifted '+' key
    Vty.KChar '-'  -> ZoomOut
    Vty.KChar '_'  -> ZoomOut           -- shifted '-' key
    Vty.KChar 'r'  -> ResetView
    Vty.KChar '['  -> FramePrev
    Vty.KChar ']'  -> FrameNext
    Vty.KChar 'j'  -> FramePrev
    Vty.KChar 'k'  -> FrameNext
    _              -> NoOp
evToCmd _ = NoOp
