module Main where

import Children
import GUI

main = later waitForChildren $ do
    forkChild guiMain
