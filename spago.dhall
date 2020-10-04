{ name = "fixed-points"
, dependencies =
  [ "console"
  , "effect"
  , "exists"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
