{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "chanterelle-halogen-template"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "arrays"
  , "chanterelle"
  , "console"
  , "effect"
  , "either"
  , "eth-core"
  , "exceptions"
  , "halogen"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "strings"
  , "tagged"
  , "transformers"
  , "web-dom"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "purs/src/**/*.purs" ]
}
