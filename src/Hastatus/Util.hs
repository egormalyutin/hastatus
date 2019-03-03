module Hastatus.Util where

{-# LANGUAGE RankNTypes #-}

import Control.Monad.Trans
import Control.Concurrent.MVar
import Conduit

newtype Mutex = Mutex { runMutex :: MVar () }

createMutex :: MonadIO m => m Mutex
createMutex = return . Mutex =<< liftIO newEmptyMVar

lockMutex :: MonadIO m => Mutex -> m ()
lockMutex (Mutex v) = liftIO $ putMVar v ()

unlockMutex :: MonadIO m => Mutex -> m ()
unlockMutex (Mutex v) = liftIO $ takeMVar v

-- | Concat list of string
renderList :: [String] -> String
renderList = concat

-- | Conduit which does nothing with data, just sends it downstream.
noopConduit :: Monad m => ConduitT i i m ()
noopConduit = c
    where c = do
            a <- await
            case a of
                Just v -> do
                    yield v
                    c

                Nothing -> return ()

-- | Alias to noopConduit
noopC :: Monad m => ConduitT i i m ()
noopC = noopConduit
