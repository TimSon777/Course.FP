module Main where

import MyLib
import System.Environment (getArgs)
import Control.Monad.Reader (runReaderT)

main :: IO ()
main = do
    args <- getArgs
    let config = MyLib.parseArgs args
    if verbose config then 
      putStrLn $ show config
    else
      return ()
    runReaderT calculateDiskUsage config