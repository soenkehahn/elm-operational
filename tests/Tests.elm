module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import OperationalSpec


all : Test
all =
    describe "operational"
        [ OperationalSpec.all
        ]
