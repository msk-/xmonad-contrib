-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.EasyMotion
-- Copyright   :  (c) Matt Kingston <mattkingston@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  mattkingston@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides functionality to use key chords to focus a visible window. Overlays a unique key chord
-- (a string) above each visible window and allows the user to select a window by typing that
-- chord.
-- Inspired by https://github.com/easymotion/vim-easymotion.
--
-----------------------------------------------------------------------------

module XMonad.Actions.EasyMotion (
                                   -- * Usage
                                   -- $usage
                                   selectWindow
                                 , def
                                 , EasyMotionConfig(..)
                                 ) where

import XMonad
import XMonad.StackSet as W
import XMonad.Util.Font (releaseXMF, initXMF, Align(AlignCenter), XMonadFont(..))
import XMonad.Util.XUtils (fi, createNewWindow, paintAndWrite, deleteWindow, showWindow)
import Control.Applicative ((<$>))
import Control.Monad (when, replicateM)
import Data.Maybe (isJust)
import Data.Set (fromList, toList)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (getWindowAttributes, getEvent)
import qualified Data.List as L (filter, foldl', partition, find)

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.EasyMotion (selectWindow)
--
-- Then add a keybinding to the selectWindow action:
--
-- >    , ((modm, xK_f), selectWindow def)
--
-- Default chord keys are s,d,f,j,k,l. To customise these and display options assign
-- different values to def:
--
-- >    import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..))
-- >    , ((modm, xK_f), selectWindow def {emKeys = [xK_f, xK_d], emFont: "xft: Sans-40" })
--
-- You should supply at least two keys in the emKeys list.
--
-- The font field provided is supplied directly to the initXMF function. The default is
-- "xft:Sans-100". Some example options:
--
-- >    "xft: Sans-40"
-- >    "xft: Arial-100"
-- >    "xft: Cambria-80"

-- TODO:
--  - Use stringToKeysym, keysymToKeycode, keycodeToKeysym, keysymToString to take a string from
--    the user?
--  - Think a bit more about improving functionality with floating windows.
--    - currently, floating window z-order is not respected
--    - could ignore floating windows
--    - may be able to calculate the visible section of a floating window, and display the chord in
--      that space
--  - Provide an option to prepend the screen key to the easymotion keys (i.e. w,e,r)?
--  - overlay alpha
--  - Make a 'bring that window to this workspace' option. i.e. a window bringer and a window goto.
--    How modular is the WindowBringer extension? Can we provide a window-selector function to it?
--  - Attempt to order windows left-to-right, top-to-bottom, then match select keys with them such
--    that for keys asdf the left-top-most window has key a, etc.
--  - Provide multiple lists of keys, one for each screen. This way one could learn to use certain
--    keys for certain screens. In the case of a two-screen setup, this could also be used to map
--    hands to screens.
--  - Combining the above two options should make it possible to, for any given layout and number
--    of windows, predict the key that will be required to select a given window.
--  - Allow the user to limit chord length; even if this means that not all windows are given a
--    chord.
--  - Delay after selection so the user can see what they've chosen? Min-delay: 0 seconds. If
--    there's a delay, perhaps keep the other windows covered briefly to naturally draw the user's
--    attention to the window they've selected? Or briefly highlight the border of the selected
--    window?
--  - Something unpleasant happens when the user provides only two keys (let's say f, d) for
--    chords. When they have five windows open, the following chords are generated: ddd, ddf, dfd,
--    dff, fdd. When 'f' is pressed, all chords disappear unexpectedly because we know there are no
--    other valid options. The user expects to press 'fdd'. This is an optimisation in software but
--    pretty bad for usability, as the user continues firing keys into their
--    now-unexpectedly-active window. And is of course only one concrete example of a more general
--    problem.

-- | Associates a user window, an overlay window created by this module, a rectangle circumscribing
--   these windows, and the chord that will be used to select them
data Overlay = 
  Overlay { win     :: Window      -- The window managed by xmonad
          , overlay :: Window      -- Our window used to display the overlay
          , rect    :: Rectangle   -- The rectangle of 'win (and 'overlay)
          , chord   :: [KeySym]    -- The chord we'll display in the overlay
          } deriving (Show, Eq)

-- | Configuration options for EasyMotion
data EasyMotionConfig =
  EMConf { emTextColor   :: String   -- ^ Color of the text displayed
         , emBgColor     :: String   -- ^ Color of the window overlaid
         , emBorderColor :: String   -- ^ Color of the overlay window borders
         , emKeys        :: [KeySym] -- ^ Keys to use for window selection
         , emCancelKey   :: KeySym   -- ^ Key to use to cancel selection
         , emFont        :: String   -- ^ Font for selection characters (passed to initXMF)
         , emBorderWidth :: Int      -- ^ Width of border in pixels
         , emMaxChordLen :: Int      -- ^ Maximum chord length. Use 0 for no maximum.
         } deriving (Show)

instance Default EasyMotionConfig where
  def =
    EMConf { emTextColor   = "#ffffff"
           , emBgColor     = "#000000"
           , emBorderColor = "#ffffff"
           , emKeys        = [xK_s, xK_d, xK_f, xK_j, xK_k, xK_l]
           , emCancelKey   = xK_q
           , emFont        = "xft: Sans-100"
           , emBorderWidth = 1
           , emMaxChordLen = -1
           }

-- | Display overlay windows and chords for window selection
selectWindow :: EasyMotionConfig -> X ()
selectWindow EMConf { emKeys = [] } = return ()
selectWindow EMConf { emTextColor   = textColor
                    , emBgColor     = bgColor
                    , emBorderColor = borderColor
                    , emKeys        = keys
                    , emCancelKey   = cancelKey
                    , emFont        = font
                    , emBorderWidth = brW
                    , emMaxChordLen = maxChordLen
                    } = do
  -- make sure the key list doesn't contain: duplicates, 'cancelKey, backspace
  let filterKeys = toList . fromList . L.filter (fmap not (`elem` [cancelKey, xK_BackSpace]))
  case filterKeys keys of
    [] -> return ()
    [x] -> return ()
    filteredKeys -> do
      f <- initXMF font
      XConf { theRoot = rw, display = dpy } <- ask
      XState { mapped = wins, windowset = ws } <- get
      let currentW = W.stack . W.workspace . W.current $ ws
      let buildOverlay w = do
          r <- getWindowRect dpy w
          o <- createNewWindow r Nothing "" True
          return Overlay { win=w, rect=r, overlay=o }
      overlays <- appendChords maxChordLen filteredKeys <$> sequence (fmap buildOverlay (toList wins))
      status <- io $ grabKeyboard dpy rw True grabModeAsync grabModeAsync currentTime
      when (status == grabSuccess) $ do
          -- handle keyboard input
          resultWin <- handle dpy (displayOverlay f bgColor borderColor textColor brW) cancelKey overlays []
          io $ ungrabKeyboard dpy currentTime
          mapM_ (deleteWindow . overlay) overlays
          case resultWin of
            Selected o -> windows . W.focusWindow . win $ o
            _ -> whenJust currentW (windows . W.focusWindow . W.focus) -- return focus correctly
          io $ sync dpy False
          releaseXMF f

-- | Take a list of overlays lacking chords, return a list of overlays with key chords
appendChords :: Int -> [KeySym] -> [Overlay] -> [Overlay]
appendChords maxLen keys os =
  zipWith (\c o -> Overlay { overlay=overlay o, rect=rect o, chord=c, win=win o }) chords os
    where
      chords = replicateM chordLen keys
      tempLen = (fi . length $ os) `div` (fi . length $ keys) + 1
      chordLen = if maxLen <= 0 then tempLen else min tempLen maxLen

-- | Get a key event, translate it to an event type and keysym
event d = allocaXEvent $ \e -> do
  maskEvent d (keyPressMask .|. keyReleaseMask) e
  KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent e
  s <- keycodeToKeysym d c 0
  return (t, s)

-- | A three-state result for handling user-initiated selection cancellation, successful selection,
--   or backspace.
data HandleResult a = Exit | Selected a | Backspace
-- | Handle key press events for window selection.
handle :: Display -> (Overlay -> X()) -> KeySym -> [Overlay] -> [Overlay] -> X (HandleResult Overlay)
handle dpy drawFn cancelKey fgOverlays bgOverlays = do
  let redraw = mapM (mapM_ drawFn) [fgOverlays, bgOverlays]
  let retryBackspace x =
        case x of
          Backspace -> do redraw
                          handle dpy drawFn cancelKey fgOverlays bgOverlays
          _ -> return x
  redraw
  (t, s) <- io $ event dpy
  case () of
    () | t == keyPress && s == cancelKey -> return Exit
    () | t == keyPress && s == xK_BackSpace -> return Backspace
    () | t == keyPress && isJust (L.find ((== s) . head .chord) fgOverlays) ->
      case fg of
        [x] -> return $ Selected x
        _   -> handle dpy drawFn cancelKey (trim fg) (clear bg) >>= retryBackspace
      where
        (fg, bg) = L.partition ((== s) . head . chord) fgOverlays
        trim = map (\o -> o { chord = tail $ chord o })
        clear = map (\o -> o { chord = [] })
    () -> handle dpy drawFn cancelKey fgOverlays bgOverlays

-- | Get the circumscribing rectangle of the given X window
getWindowRect :: Display -> Window -> X Rectangle
getWindowRect dpy w = io $ fmap makeRect (getWindowAttributes dpy w)
  where
    makeRect :: WindowAttributes -> Rectangle
    makeRect wa = Rectangle (fi (wa_x wa)) (fi (wa_y wa)) (fi (wa_width wa)) (fi (wa_height wa))

-- | Display an overlay with the provided formatting
displayOverlay :: XMonadFont -> String -> String -> String -> Int -> Overlay -> X ()
displayOverlay f bgC brC textC brW Overlay { overlay = w, rect = r, chord = c } = do
  showWindow w
  paintAndWrite w f (fi (rect_width r)) (fi (rect_height r)) (fi brW) bgC brC textC bgC [AlignCenter] [L.foldl' (++) "" $ map keysymToString c]
