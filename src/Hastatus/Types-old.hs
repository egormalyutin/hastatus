{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module Hastatus.Types where

import Control.Concurrent.MVar
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- | Main monad of hastatus. Contains list of widgets.
data StatusT a = StatusT { runStatusT :: m (a, [WidgetContainer]) }

instance Monad m => Monad (StatusT m) where
    return v = StatusT (v, [])

-- | Widget is some type which can be used to get an MVar. Hastatus updates status every time when this MVar will have new value.
class Widget a where
    toMVar :: a -> IO (MVar String)

-- | Formatter is some type which can converted to function which can edit output of Widget. For example, it can change color of output.
class Formatter a where
    toFunction :: a -> (String -> IO String)

-- | Simple widget containing function that returns MVar.
data SimpleWidget = SimpleWidget (IO (MVar String))

instance Widget SimpleWidget where
    toMVar (SimpleWidget f) = f

-- | Simple formatter containing function that formats string.
data SimpleFormatter = SimpleFormatter (String -> IO String)

instance Formatter SimpleFormatter where
    toFunction (SimpleFormatter f) = f

-- | Container with any widget.
data WidgetContainer = forall a. Widget a => WidgetContainer a

instance Widget WidgetContainer where
    toMVar (WidgetContainer w) = toMVar w
