module Children where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (finally)

import System.IO.Unsafe (unsafePerformIO)

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
      cs <- takeMVar children
      case cs of
        []   -> return ()
        m:ms -> do
           putMVar children ms
           takeMVar m
           waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkIO (io `finally` putMVar mvar ())

later x y = y >> x
