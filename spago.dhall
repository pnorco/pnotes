{ name = "pnotes"
, dependencies = 
    [ "console"
    , "effect"
    , "httpure"
    , "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
