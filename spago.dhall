{ sources =
    [ "src/**/*.purs"
    , "examples/**/*.purs"
    ]
, name =
    "hedwig"
, dependencies =
    [ "aff"
    , "affjax"
    , "console"
    , "effect"
    , "foreign"
    , "nullable"
    , "prelude"
    , "random"
    , "psci-support"
    , "random"
    , "web-events"
    , "web-dom"
    , "web-html"
    , "web-socket"
    , "web-uievents"
    , "windrose-router"
    ]
, packages =
    ./packages.dhall
}
