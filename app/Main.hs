module Main where

import Control.Monad.Reader
import Lib
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir
import System.Directory

main :: IO ()
main = do
    role:pool <- getArgs
    state <- obtainState role
    putStr $ renderImprovements $ runReader (bestImprovement pool) state

stateDir :: IO String
stateDir = getUserCacheDir "champion-pool"

stateFile :: String -> IO String
stateFile role = getUserCacheFile "champion-pool" role

obtainState :: String -> IO State
obtainState role = do
    stateDir >>= createDirectoryIfMissing False
    file <- stateFile role
    exists <- doesFileExist file
    if exists
    then fmap read $ readFile file
    else do
        state <- pullState role
        writeFile file $ show state
        return state
