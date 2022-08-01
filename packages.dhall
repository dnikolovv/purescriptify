let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.7-20220418/packages.dhall sha256:67cc3d4f0e8fb72bb1413ba94ddd72a3ceb0783eb725e3b22ad7568b3b581163

let overrides = {=}

let pursTidyDeps =
      { language-cst-parser =
        { dependencies =
          [ "arrays"
          , "const"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "free"
          , "functors"
          , "maybe"
          , "numbers"
          , "ordered-collections"
          , "strings"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-language-cst-parser.git"
        , version = "v0.10.0"
        }
      , dodo-printer =
        { dependencies =
          [ "ansi", "foldable-traversable", "lists", "maybe", "strings" ]
        , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
        , version = "v2.1.0"
        }
      , node-glob-basic =
        { dependencies =
          [ "aff"
          , "console"
          , "effect"
          , "lists"
          , "maybe"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "ordered-collections"
          , "strings"
          ]
        , repo = "https://github.com/natefaubion/purescript-node-glob-basic.git"
        , version = "v1.2.2"
        }
      , node-workerbees =
        { dependencies =
          [ "aff"
          , "argonaut-core"
          , "arraybuffer-types"
          , "avar"
          , "effect"
          , "either"
          , "exceptions"
          , "maybe"
          , "newtype"
          , "parallel"
          , "variant"
          ]
        , repo = "https://github.com/natefaubion/purescript-node-workerbees.git"
        , version = "v0.2.1"
        }
      , argparse-basic =
        { dependencies =
          [ "either"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "record"
          , "strings"
          ]
        , repo = "https://github.com/natefaubion/purescript-argparse-basic.git"
        , version = "v1.0.0"
        }
      }

let pursTidy =
    { purs-tidy =
      { dependencies =
          ["arrays"
        , "control"
        , "dodo-printer"
        , "either"
        , "foldable-traversable"
        , "lists"
        , "maybe"
        , "newtype"
        , "ordered-collections"
        , "partial"
        , "prelude"
        , "language-cst-parser"
        , "strings"
        , "tuples"
          ]
      , repo =
          "https://github.com/natefaubion/purescript-tidy"
      , version =
          "ee754b1746fe347c3dc37b19908d6be56d96fc6a"
      }
  }

in pursTidyDeps // pursTidy // upstream // overrides
