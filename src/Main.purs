module Main where

import Prelude
import Day2.PasswordPhilosophy (inputPath, getSolutions)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- TODO: start working with Streams to allow for larger input
main :: Effect Unit
main = do
  file <- readTextFile UTF8 inputPath
  log $ getSolutions file