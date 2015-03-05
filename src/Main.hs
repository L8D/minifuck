module Main
    ( main
    ) where

import Control.Monad.State
import System.Environment
import Text.Parsec

import MiniFuck

runProgram :: String -> IO ()
runProgram s = case parse programParser "" s of
    Left e   -> fail (show e)
    Right is -> runStateT (runVM $ run is) initWorld >> return ()

main :: IO ()
main = getArgs >>= go where
    go []    = getContents >>= runProgram
    go (x:_) = readFile x >>= runProgram
