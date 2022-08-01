{ name = "purescriptify"
, dependencies =
  [ "arrays"
  , "console"
  , "dodo-printer"
  , "effect"
  , "either"
  , "foreign"
  , "language-cst-parser"
  , "maybe"
  , "prelude"
  , "purs-tidy"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "safe-coerce"
  , "strings"
  , "tuples"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
