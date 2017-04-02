module OperationalSpec exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Operational exposing (..)
import Tuple exposing (..)
import Array exposing (..)
import Platform.Cmd exposing (..)
import Http exposing (..)
import Platform exposing (..)


type alias Flags =
    { apiRoot : String }


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
        [ describe "toCmd"
            [ test "allows to turn commands into elm's Cmd"
                (\() ->
                    let
                        testableProgram :
                            { init : ( Model, List TestCmd )
                            , update : Msg -> Model -> ( Model, List TestCmd )
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
                            { init : ( Model, Cmd Msg )
                            , update : Msg -> Model -> ( Model, Cmd Msg )
                            , somethingElse : ()
                            }
                        runnableProgram =
                            toCmd runCmd testableProgram
                    in
                        pass
                )
            , test "works in combination with Platform.program"
                (\() ->
                    let
                        testableProgram :
                            { init : ( Model, List TestCmd )
                            , update : Msg -> Model -> ( Model, List TestCmd )
                            , subscriptions : Model -> Sub Msg
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
                            , subscriptions = \_ -> Sub.none
                            }

                        runnableProgram =
                            toCmd runCmd testableProgram

                        main : Program Never Model Msg
                        main =
                            program runnableProgram
                    in
                        pass
                )
            ]
        , describe "toCmdWithFlags"
            [ test "works in combination with Platform.program"
                (\() ->
                    let
                        testableProgram :
                            { init : Flags -> ( Model, List TestCmd )
                            , update : Msg -> Model -> ( Model, List TestCmd )
                            , subscriptions : Model -> Sub Msg
                            }
                        testableProgram =
                            { init =
                                \flags ->
                                    ( empty, [ Get (flags.apiRoot ++ "/foo") ] )
                            , update =
                                \msg model ->
                                    case msg of
                                        Error _ ->
                                            ( model, [] )

                                        Response s ->
                                            ( push s model, [ Get s ] )
                            , subscriptions = \_ -> Sub.none
                            }

                        runnableProgram =
                            toCmdWithFlags runCmd testableProgram

                        main : Program Flags Model Msg
                        main =
                            programWithFlags runnableProgram
                    in
                        pass
                )
            ]
        ]
