module Operational exposing (toCmd, (!!))

import Debug exposing (..)
import Tuple exposing (..)
import Platform.Cmd exposing (..)


(!!) : model -> List a -> ( model, List a )
(!!) model primitives =
    ( model, primitives )


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
