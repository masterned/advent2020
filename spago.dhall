{ name = "advent-of-code-2020"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "node-buffer"
  , "node-fs"
  , "psci-support"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
