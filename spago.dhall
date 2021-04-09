{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "chanterelle-halogen-template"
, dependencies =
  [ "ansi"
  , "argonaut"
  , "console"
  , "effect"
  , "errors"
  , "eth-core"
  , "fixed-points"
  , "mkdirp"
  , "node-fs-aff"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "record-extra"
  , "string-parsers"
  , "web3"
  , "yargs"
  , "halogen"
  , "affjax"
  , "chanterelle"
  ]
, packages = ./packages.dhall
, sources = [ "purs/src/**/*.purs" ]
}
