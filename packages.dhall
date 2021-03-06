let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190614/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200127/packages.dhall sha256:06a623f48c49ea1c7675fdf47f81ddb02ae274558e29f511efae1df99ea92fb8

let overrides = {=}

let additions = {
      windrose-router =
          mkPackage
            [ "aff-coroutines", "effect", "console", "halogen", "heterogeneous", "psci-support", "strings", "strongcheck" ]
            "https://github.com/blankhart/purescript-windrose-router.git"
            "v0.1"
}

in  upstream // overrides // additions
