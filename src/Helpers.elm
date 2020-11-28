module Helpers exposing (..)


withNoCmd : a -> ( a, Cmd msg )
withNoCmd x =
    ( x, Cmd.none )
