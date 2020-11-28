module Backend exposing (app)

import Cards exposing (Cards)
import Dict exposing (Dict)
import Helpers exposing (withCmd, withNoCmd)
import Lamdera exposing (ClientId, SessionId)
import Random
import Repo exposing (Id, Repo)
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    { games = Repo.empty
    , waitingRoom = Nothing
    }
        |> withCmd (generateSeed GotSeedForWaitingRoom)


initWaitingRoom : Random.Seed -> WaitingRoom
initWaitingRoom seed =
    { players = Repo.empty
    , cards = Cards.new seed
    }


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOpBackendMsg ->
            noOp

        GotSeedForWaitingRoom seed ->
            { model | waitingRoom = Just <| initWaitingRoom seed }
                |> withNoCmd


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOpToBackend ->
            noOp



-- HELPERS


generateSeed : (Random.Seed -> msg) -> Cmd msg
generateSeed msg =
    Random.independentSeed |> Random.generate msg
