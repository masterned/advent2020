{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "advent-of-code-2020"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "int-53"
  , "lists"
  , "node-fs-aff"
  , "partial"
  , "psci-support"
  , "record"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
