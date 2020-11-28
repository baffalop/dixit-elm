module Helpers exposing (..)

import Basics.Extra exposing (flip)


withCmd : Cmd msg -> a -> ( a, Cmd msg )
withCmd =
    flip Tuple.pair


withNoCmd : a -> ( a, Cmd msg )
withNoCmd =
    withCmd Cmd.none
