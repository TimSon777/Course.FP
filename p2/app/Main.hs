module Main where

import MyLib (parseCSV)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad

main :: IO ()
main = do
    let input = "Name,Age\nTimur,21\nDanil,22"
    case parseCSV input ',' True of
        Left err -> putStr (errorBundlePretty err)
        Right (header, content) -> do
            print header
            print content
