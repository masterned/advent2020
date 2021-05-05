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
  , "lists"
  , "node-fs-aff"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "psci-support"
  , "record"
  , "test-unit"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
