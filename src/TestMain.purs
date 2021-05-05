module Test.Main where

import Prelude
import Effect (Effect)
import Day1.Test.ReportRepair (runTests)

main :: Effect Unit
main = runTests
