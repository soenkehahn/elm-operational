module OperationalSpec exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Operational exposing (..)
import Tuple exposing (..)
import Array exposing (..)
import Platform.Cmd exposing (..)
import Http exposing (..)


type Msg
    = FooResponse String
    | Error Http.Error


type TestPrimitive
    = Get String


mkProgram { init, update } =
    { init =
        ( empty, init )
    , update =
        \msg model ->
            case msg of
                Error _ ->
                    ( model, [] )

                FooResponse s ->
                    ( push s model, update s )
    }


all : Test
all =
    describe "Operational"
        [ test "allows to turn commands into elm's Cmd"
            (\() ->
                let
                    p :
                        { init : ( Array String, List TestPrimitive )
                        , update : Msg -> Array String -> ( Array String, List TestPrimitive )
                        , somethingElse : ()
                        }
                    p =
                        let
                            inner =
                                mkProgram
                                    { init = [ Get "/foo" ]
                                    , update = \_ -> []
                                    }
                        in
                            { init = inner.init, update = inner.update, somethingElse = () }

                    convert : TestPrimitive -> Platform.Cmd.Cmd Msg
                    convert p =
                        case p of
                            Get url ->
                                let
                                    handleResponse result =
                                        case result of
                                            Ok x ->
                                                FooResponse x

                                            Err err ->
                                                Error err
                                in
                                    send handleResponse (getString url)

                    x :
                        { init : ( Array String, Cmd Msg )
                        , update : Msg -> Array String -> ( Array String, Cmd Msg )
                        , somethingElse : ()
                        }
                    x =
                        toCmd convert p
                in
                    pass
            )
        ]
