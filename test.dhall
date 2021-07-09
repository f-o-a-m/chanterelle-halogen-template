let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "purs/test/**/*.purs" ],
  dependencies = conf.dependencies # [ "spec", "avar", "node-process", "unsafe-coerce" ]
}
