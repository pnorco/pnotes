{ name = "pnotes"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "httpure"
  , "maybe"
  , "node-fs-aff"
  , "prelude"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
