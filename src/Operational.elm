module Operational exposing (toCmd, (!!))

{-| This module allows to write elm components without using the usual `Cmd`
type. This is useful for being able test commands that apps perform using
`elm-operational-mocks`.

# Converting to `Cmd`

@docs toCmd

# Convenience functions
@docs (!!)

-}

import Debug exposing (..)
import Tuple exposing (..)
import Platform.Cmd exposing (..)


{-| `toCmd` can be used to turn components that are defined in terms of your own
primitive commands into a proper elm component where `init` and `update` return
operations of type `Cmd`.
-}
toCmd :
    (primitive -> Cmd msg)
    -> { program
        | init : ( model, List primitive )
        , update : msg -> model -> ( model, List primitive )
       }
    -> { program
        | init : ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
       }
toCmd f program =
    let
        convert ( a, primitives ) =
            ( a
            , Cmd.batch (List.map f primitives)
            )
    in
        { program
            | init = convert program.init
            , update = \msg model -> convert (program.update msg model)
        }


{-| Convenience function in analogy to `Platform.Cmd.!`.

    (model, primitiveCommands) === model !! primitiveCommands

-}
(!!) : model -> List a -> ( model, List a )
(!!) model primitives =
    ( model, primitives )
