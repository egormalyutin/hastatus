------------------------------------------------------------------------------------------------------------------
-- |
-- COPIED FROM https://hackage.haskell.org/package/taffybar-0.4.2/docs/src/System-Information-EWMHDesktopInfo.html
------------------------------------------------------------------------------------------------------------------

module Hastatus.Widgets.EWMH.Internal where

import Codec.Binary.UTF8.String as UTF8
import Control.Monad.Reader
import Data.Bits (testBit, (.|.))
import Data.List.Split (endBy)
import Data.Maybe (fromMaybe)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Data.List (elemIndex)
import Data.Maybe (listToMaybe, mapMaybe)

data X11Context = X11Context { contextDisplay :: Display, contextRoot :: Window }
type X11Property a = ReaderT X11Context IO a
type X11Window = Window
type PropertyFetcher a = Display -> Atom -> Window -> IO (Maybe [a])

-- | Put the current display and root window objects inside a Reader
-- transformer for further computation.
withDefaultCtx :: X11Property a -> IO a
withDefaultCtx fun = do
  ctx <- getDefaultCtx
  res <- runReaderT fun ctx
  closeDisplay (contextDisplay ctx)
  return res

-- | Retrieve the property of the given window (or the root window,
-- if Nothing) with the given name as a value of type Int. If that
-- property hasn't been set, then return -1.
readAsInt :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
          -> String -- ^ name of the property to retrieve
          -> X11Property Int
readAsInt window name = do
  prop <- fetch getWindowProperty32 window name
  case prop of
    Just (x:_) -> return (fromIntegral x)
    _          -> return (-1)

-- | Retrieve the property of the given window (or the root window,
-- if Nothing) with the given name as a String. If the property
-- hasn't been set, then return an empty string.
readAsString :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
             -> String          -- ^ name of the property to retrieve
             -> X11Property String
readAsString window name = do
  prop <- fetch getWindowProperty8 window name
  case prop of
    Just xs -> return . UTF8.decode . map fromIntegral $ xs
    _       -> return []

-- | Retrieve the property of the given window (or the root window,
-- if Nothing) with the given name as a list of Strings. If the
-- property hasn't been set, then return an empty list.
readAsListOfString :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
                   -> String          -- ^ name of the property to retrieve
                   -> X11Property [String]
readAsListOfString window name = do
  prop <- fetch getWindowProperty8 window name
  case prop of
    Just xs -> return (parse xs)
    _       -> return []
  where
    parse = endBy "\0" . UTF8.decode . map fromIntegral

-- | Retrieve the property of the given window (or the root window,
-- if Nothing) with the given name as a list of X11 Window IDs. If
-- the property hasn't been set, then return an empty list.
readAsListOfWindow :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
                   -> String          -- ^ name of the property to retrieve
                   -> X11Property [X11Window]
readAsListOfWindow window name = do
  prop <- fetch getWindowProperty32 window name
  case prop of
    Just xs -> return $ map fromIntegral xs
    _       -> return []

-- | Determine whether the \"urgent\" flag is set in the WM_HINTS of
-- the given window.
isWindowUrgent :: X11Window -> X11Property Bool
isWindowUrgent window = do
  hints <- fetchWindowHints window
  return $ testBit (wmh_flags hints) urgencyHintBit

-- | Retrieve the value of the special _XMONAD_VISIBLE_WORKSPACES hint set
-- by the PagerHints hook provided by Taffybar (see module documentation for
-- instructions on how to do this), or an empty list of strings if the
-- PagerHints hook is not available.
getVisibleTags :: X11Property [String]
getVisibleTags = return =<<
  readAsListOfString Nothing "_XMONAD_VISIBLE_WORKSPACES"

-- | Return the Atom with the given name.
getAtom :: String -> X11Property Atom
getAtom s = do
  (X11Context d _) <- ask
  atom <- liftIO $ internAtom d s False
  return atom

-- | Spawn a new thread and listen inside it to all incoming events,
-- invoking the given function to every event of type @MapNotifyEvent@ that
-- arrives, and subscribing to all events of this type emitted by newly
-- created windows.
eventLoop :: (Event -> IO ()) -> X11Property ()
eventLoop dispatch = do
  (X11Context d w) <- ask
  liftIO $ do
    xSetErrorHandler
    selectInput d w $ propertyChangeMask .|. substructureNotifyMask
    allocaXEvent $ \e -> forever $ do
      event <- nextEvent d e >> getEvent e
      case event of
        MapNotifyEvent _ _ _ _ _ window _ -> do
          selectInput d window propertyChangeMask
        _ -> return ()
      dispatch event

-- | Emit a \"command\" event with one argument for the X server. This is
-- used to send events that can be received by event hooks in the XMonad
-- process and acted upon in that context.
sendCommandEvent :: Atom -> Atom -> X11Property ()
sendCommandEvent cmd arg = do
  (X11Context dpy root) <- ask
  sendCustomEvent dpy cmd arg root root

-- | Similar to 'sendCommandEvent', but with an argument of type Window.
sendWindowEvent :: Atom -> X11Window -> X11Property ()
sendWindowEvent cmd win = do
  (X11Context dpy root) <- ask
  sendCustomEvent dpy cmd cmd root win

-- | Build a new X11Context containing the current X11 display and its root
-- window.
getDefaultCtx :: IO X11Context
getDefaultCtx = do
  d <- openDisplay ""
  w <- rootWindow d $ defaultScreen d
  return $ X11Context d w

-- | Apply the given function to the given window in order to obtain the X11
-- property with the given name, or Nothing if no such property can be read.
fetch :: (Integral a)
      => PropertyFetcher a -- ^ Function to use to retrieve the property.
      -> Maybe X11Window   -- ^ Window to read from. Nothing means the root Window.
      -> String            -- ^ Name of the property to retrieve.
      -> X11Property (Maybe [a])
fetch fetcher window name = do
  (X11Context dpy root) <- ask
  atom <- getAtom name
  prop <- liftIO $ fetcher dpy atom (fromMaybe root window)
  return prop

-- | Retrieve the @WM_HINTS@ mask assigned by the X server to the given window.
fetchWindowHints :: X11Window -> X11Property WMHints
fetchWindowHints window = do
  (X11Context d _) <- ask
  hints <- liftIO $ getWMHints d window
  return hints

-- | Emit an event of type @ClientMessage@ that can be listened to and
-- consumed by XMonad event hooks.
sendCustomEvent :: Display
                -> Atom
                -> Atom
                -> X11Window
                -> X11Window
                -> X11Property ()
sendCustomEvent dpy cmd arg root win = do
  liftIO $ allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e win cmd 32 arg currentTime
    sendEvent dpy root False structureNotifyMask e
    sync dpy False

-- | Convenience alias for a pair of the form (props, window), where props is a
-- tuple of the form (workspace index, window title, window class), and window
-- is the internal ID of an open window.
type X11WindowHandle = ((Int, String, String), X11Window)

noFocus :: String
noFocus = "..."

-- | Retrieve the index of the current workspace in the desktop,
-- starting from 0.
getCurrentWorkspace :: X11Property Int
getCurrentWorkspace = readAsInt Nothing "_NET_CURRENT_DESKTOP"

-- | Retrieve the indexes of all currently visible workspaces
-- with the active workspace at the head of the list.
getVisibleWorkspaces :: X11Property [Int]
getVisibleWorkspaces = do
  vis <- getVisibleTags
  allNames <- getWorkspaceNames
  cur <- getCurrentWorkspace
  return $ cur : mapMaybe (`elemIndex` allNames) vis

-- | Return a list with the names of all the workspaces currently
-- available.
getWorkspaceNames :: X11Property [String]
getWorkspaceNames = readAsListOfString Nothing "_NET_DESKTOP_NAMES"

-- | Ask the window manager to switch to the workspace with the given
-- index, starting from 0.
switchToWorkspace :: Int -> X11Property ()
switchToWorkspace idx = do
  cmd <- getAtom "_NET_CURRENT_DESKTOP"
  sendCommandEvent cmd (fromIntegral idx)

-- | Get the title of the given X11 window.
getWindowTitle :: X11Window -> X11Property String
getWindowTitle window = do
  let w = Just window
  prop <- readAsString w "_NET_WM_NAME"
  case prop of
    "" -> readAsString w "WM_NAME"
    _  -> return prop

-- | Get the class of the given X11 window.
getWindowClass :: X11Window -> X11Property String
getWindowClass window = readAsString (Just window) "WM_CLASS"

withActiveWindow :: (X11Window -> X11Property String) -> X11Property String
withActiveWindow getProp = do
  awt <- readAsListOfWindow Nothing "_NET_ACTIVE_WINDOW"
  let w = listToMaybe $ filter (>0) awt
  maybe (return noFocus) getProp w

-- | Get the title of the currently focused window.
getActiveWindowTitle :: X11Property String
getActiveWindowTitle = withActiveWindow getWindowTitle

-- | Return a list of all windows
getWindows :: X11Property [X11Window]
getWindows = readAsListOfWindow Nothing "_NET_CLIENT_LIST"

-- | Return a list of X11 window handles, one for each window open. Refer to the
-- documentation of 'X11WindowHandle' for details on the structure returned.
getWindowHandles :: X11Property [X11WindowHandle]
getWindowHandles = do
  windows <- getWindows
  workspaces <- mapM getWorkspace windows
  wtitles <- mapM getWindowTitle windows
  wclasses <- mapM getWindowClass windows
  return $ zip (zip3 workspaces wtitles wclasses) windows

-- | Return the index (starting from 0) of the workspace on which the
-- given window is being displayed.
getWorkspace :: X11Window -> X11Property Int
getWorkspace window = readAsInt (Just window) "_NET_WM_DESKTOP"

-- | Ask the window manager to give focus to the given window.
focusWindow :: X11Window -> X11Property ()
focusWindow wh = do
  cmd <- getAtom "_NET_ACTIVE_WINDOW"
  sendWindowEvent cmd (fromIntegral wh)
