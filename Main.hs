module Main where

import Foreign.Marshal.Alloc
import System.Log.FastLogger

main :: IO ()
main = do
  -- Direct leak
  mallocBytes 1024
  -- Leak from a library
  newFastLogger (LogStdout 1024)
  return ()
