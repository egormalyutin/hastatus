module Hastatus.Types where

import Control.Monad.Trans
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Reader.Class
import Control.Monad
import Conduit hiding ((=$))

import Control.Concurrent hiding (yield)
import Control.Concurrent.MVar
import Data.IORef

import Hastatus.Util

-- | Output is conduit which produces a new string with certain interval. Output is base of hastatus widgets
type Output = ConduitT () String IO ()

-- | Formatter is conduit which modifies strings. It can change color of output, for example
type Formatter = ConduitT String String IO ()

-- | Status is main monad of hastatus. It allows to create widgets and combine them to one status line
type Status = WriterT [Output] IO

-- | Get outputs from Status
getOutputs :: Status a -> IO [Output]
getOutputs = execWriterT

type Widget = Status ()

-- | Simple helper to create widget from Output
widget :: Output -> Status ()
widget o = writer ((), [o])

infixr 9 =$, =$$

-- | Apply Formatter on each Output of Status
(=$) :: Formatter -> Status a -> Status a
(=$) f = censor $ map (.|f)

-- | Group Outputs from Status to Status with one Output
group :: Status a -> Status a
group = censor $ \inputs -> return $ do
    res <- liftIO . newMVar $ Just ""

    liftIO . forkIO $ do
        mutex <- createMutex
        strs <- mapM (\_ -> (newIORef "")) inputs
        closed <- newIORef 0

        (flip mapM_) (zip strs inputs) $ \(ref, input) -> do
            forkIO . runConduit $ input .| do
                let r = do
                        val <- await

                        case val of
                            Just str -> do
                                lockMutex mutex
                                liftIO $ writeIORef ref str

                                list <- liftIO $ mapM readIORef strs
                                liftIO . putMVar res . Just $ renderList list

                                unlockMutex mutex
                                r

                            Nothing -> do
                                liftIO $ modifyIORef closed (+1)
                                closedCount <- liftIO $ readIORef closed
                                when (closedCount == length inputs) . liftIO $ putMVar res Nothing
                                return ()
                r

            return ()

        return ()

    let r = do
            val <- liftIO $ takeMVar res
            case val of
                Just str -> do
                    yield str
                    r

                Nothing -> return ()

    r

-- | Group Outputs form Status to Status with one Output and apply Formatter on it
(=$$) :: Formatter -> Status a -> Status a
f =$$ s = f =$ (group s)
