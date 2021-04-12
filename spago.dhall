{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "chanterelle-halogen-template"
, dependencies =
  [ "ansi"
  , "console"
  , "effect"
  , "errors"
  , "eth-core"
  , "mkdirp"
  , "node-fs-aff"
  , "prelude"
  , "web3"
  , "halogen"
  , "affjax"
  , "chanterelle"
  ]
, packages = ./packages.dhall
, sources = [ "purs/src/**/*.purs" ]
}
