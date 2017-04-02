module OperationalSpec exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Operational exposing (..)
import Tuple exposing (..)
import Array exposing (..)
import Platform.Cmd exposing (..)
import Http exposing (..)


type alias Model =
    Array String


type Msg
    = Response String
    | Error Http.Error


type TestCmd
    = Get String


runCmd : TestCmd -> Platform.Cmd.Cmd Msg
runCmd p =
    case p of
        Get url ->
            let
                handleResponse result =
                    case result of
                        Ok x ->
                            Response x

                        Err err ->
                            Error err
            in
                send handleResponse (getString url)


all : Test
all =
    describe "Operational"
        [ test "allows to turn commands into elm's Cmd"
            (\() ->
                let
                    testableProgram :
                        { init : ( Array String, List TestCmd )
                        , update : Msg -> Array String -> ( Array String, List TestCmd )
                        , somethingElse : ()
                        }
                    testableProgram =
                        { init =
                            ( empty, [ Get "/foo" ] )
                        , update =
                            \msg model ->
                                case msg of
                                    Error _ ->
                                        ( model, [] )

                                    Response s ->
                                        ( push s model, [ Get s ] )
                        , somethingElse = ()
                        }

                    runnableProgram :
                        { init : ( Array String, Cmd Msg )
                        , update : Msg -> Array String -> ( Array String, Cmd Msg )
                        , somethingElse : ()
                        }
                    runnableProgram =
                        toCmd runCmd testableProgram
                in
                    pass
            )
        ]
