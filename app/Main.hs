module Main where

import Hastatus
import Control.Concurrent hiding (yield)

stupid :: Widget
stupid = widget $ do
    let c i = do
            yield $ show i
            liftIO $ threadDelay 100000
            c $ i + 1
    c 0

form :: Formatter
form = noopConduit

main :: IO ()
main = do
    logStatus $ do
        stupid
        form =$$ do
            form =$ stupid
            form =$ form =$ stupid

