module Main where

import Control.Concurrent.Chan
import Control.Monad (when)

import Children
import GUI

main = later waitForChildren $ do
    chan <- newChan
    forkChild (guiMain chan)
    monitor chan

monitor chan = do
    x <- readChan chan
    putStrLn $ "Received message: " ++ x
    when (x /= "quit") $ monitor chan
