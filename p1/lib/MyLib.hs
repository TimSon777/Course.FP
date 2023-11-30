{-# LANGUAGE OverloadedStrings #-}

module MyLib (parseArgs, calculateDiskUsage, Config, verbose) where

import Control.Monad (forM_, when, unless)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, getDirectoryContents, getFileSize)
import System.FilePath ((</>), takeExtensions)
import Text.Read (readMaybe)
import Data.List (isPrefixOf)

isInt s = case readMaybe s :: Maybe Int of
    Just _  -> True
    Nothing -> False
    
data Config = Config {
    maxDepth    :: Maybe Int,
    humanReadable :: Bool,
    verbose     :: Bool,
    startDir    :: FilePath
} deriving(Show)

type App = ReaderT Config IO

parseArgs :: [String] -> Config
parseArgs args = foldr addToConfig (Config Nothing False False ".") args
  where
    addToConfig "-h" cfg = cfg { humanReadable = True }
    addToConfig "-v" cfg = cfg { verbose = True }
    addToConfig "-s" cfg = cfg { maxDepth = Just 0 }
    addToConfig "-d" cfg = cfg 
    addToConfig val cfg
        | isInt val && fromMaybe True ((> 0) <$> maxDepth cfg) = cfg { maxDepth = Just $ read val }
        | otherwise = cfg { startDir = val }

calculateDiskUsage :: App ()
calculateDiskUsage = do
    cfg <- ask
    let dir = startDir cfg
    exists <- liftIO $ doesDirectoryExist dir
    when exists $ calculateFolderSize dir 0

calculateFolderSize :: FilePath -> Int -> App ()
calculateFolderSize path depth = do
    cfg <- ask
    let mDepth = maxDepth cfg
    size <- liftIO $ getFolderSize path
    liftIO $ unless (mDepth < Just depth) $ do
        putStrLn $ path ++ " -> " ++ showSize size cfg
    contents <- liftIO $ listDirectory path
    forM_ contents $ \name -> do
        let newPath = path </> name
        isDir <- liftIO $ doesDirectoryExist newPath
        when (isDir && fromMaybe True (fmap (> depth) mDepth)) $ do
            calculateFolderSize newPath (depth + 1)

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = do
    names <- getDirectoryContents path
    return $ filter (`notElem` [".", ".."]) names

getFolderSize :: FilePath -> IO Integer
getFolderSize path = do
    contents <- listDirectory path
    sizes <- mapM (getFileSize . (path </>)) contents
    return $ sum sizes

showSize :: Integer -> Config -> String
showSize size cfg
  | humanReadable cfg = showHumanSize size
  | otherwise         = show size

showHumanSize :: Integer -> String
showHumanSize size
  | size < 1024 = show size ++ " B"
  | size < 1024^2 = show (size `div` 1024) ++ " K"
  | size < 1024^3 = show (size `div` 1024^2) ++ " M"
  | otherwise = show (size `div` 1024^3) ++ " G"