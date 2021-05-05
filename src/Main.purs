module Main where

import Prelude
import Day1.ReportRepair (answer, inputPath)
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- TODO: start working with Streams to allow for larger input
main :: Effect Unit
main = do
  file <- readTextFile UTF8 inputPath
  logShow $ answer file
