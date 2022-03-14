{ name = "pnotes"
, dependencies =
  [ "aff"
  , "arrays"
  , "ask"
  , "console"
  , "const"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foreign"
  , "formatters"
  , "httpure"
  , "maybe"
  , "node-fs-aff"
  , "node-sqlite3"
  , "now"
  , "prelude"
  , "simple-json"
  , "spork"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
