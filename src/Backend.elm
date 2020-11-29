module Backend exposing (app)

import Basics.Extra exposing (flip)
import Cards exposing (Cards)
import Dict exposing (Dict)
import Helpers exposing (withCmd, withNoCmd)
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
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
    ( { games = Repo.empty
      , waitingRoom = Nothing
      }
    , generateSeed GotSeedForWaitingRoom
    )


initWaitingRoom : Random.Seed -> WaitingRoom
initWaitingRoom seed =
    { players = Dict.empty
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

        NewPlayerJoined name ->
            case
                model.waitingRoom
                    |> Result.fromMaybe NoWaitingRoom
                    |> Result.andThen (addNewPlayer name clientId)
            of
                Ok newWaitingRoom ->
                    -- todo: check waiting room capacity
                    ( { model | waitingRoom = Just newWaitingRoom }
                    , broadcastToWaitingRoom newWaitingRoom <| PlayerHasJoined name
                    )

                Err joinError ->
                    ( model
                    , sendToFrontend clientId <| PlayerCouldNotJoin joinError
                    )


addNewPlayer : PlayerName -> ClientId -> WaitingRoom -> Result PlayerJoinError WaitingRoom
addNewPlayer name clientId ({ players, cards } as waitingRoom) =
    if Dict.member name players then
        Err DuplicatePlayerName

    else
        makePlayer name clientId cards
            |> Result.fromMaybe CouldNotDealIn
            |> Result.map
                (\( newPlayer, newCards ) ->
                    { waitingRoom
                        | players = Dict.insert name newPlayer players
                        , cards = newCards
                    }
                )


makePlayer : PlayerName -> ClientId -> Cards -> Maybe ( Player, Cards )
makePlayer name clientId cards =
    Cards.dealIn cards
        |> Maybe.map
            (Tuple.mapFirst
                (\handId ->
                    { name = name
                    , clientId = clientId
                    , score = 0
                    , handId = handId
                    }
                )
            )


broadcastToWaitingRoom : WaitingRoom -> ToFrontend -> Cmd msg
broadcastToWaitingRoom { players } msg =
    players
        |> Dict.values
        |> List.map (.clientId >> flip sendToFrontend msg)
        |> Cmd.batch



-- HELPERS


generateSeed : (Random.Seed -> msg) -> Cmd msg
generateSeed msg =
    Random.independentSeed |> Random.generate msg
