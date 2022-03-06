{ name = "pnotes"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "const"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "formatters"
  , "httpure"
  , "maybe"
  , "node-fs-aff"
  , "now"
  , "prelude"
  , "spork"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
