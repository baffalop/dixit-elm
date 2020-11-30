module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Cards exposing (Cards, Table)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Random
import Repo exposing (Id, Repo)
import Url exposing (Url)


type alias FrontendModel =
    { url : Url.Url
    , key : Nav.Key
    , model : FrontendState
    }


type FrontendState
    = LoggingIn
        { name : String
        }
    | LoadingLogin
        { name : String
        }
    | InWaitingRoom WaitingRoomData


type alias WaitingRoomData =
    { name : PlayerName
    , cards : Cards.CardList
    , players : List PlayerName
    }


type alias BackendModel =
    { games : Repo Game
    , waitingRoom : Maybe WaitingRoom
    }


type FrontendMsg
    = NoOpFrontendMsg
    | UrlClicked UrlRequest
    | UrlChanged Url
    | LoginSubmitted


type ToBackend
    = NoOpToBackend
    | NewPlayerJoined PlayerName


type BackendMsg
    = NoOpBackendMsg
    | GotSeedForWaitingRoom Random.Seed


type ToFrontend
    = NoOpToFrontend
    | PlayerCouldNotJoin PlayerJoinError
    | PlayerHasJoined PlayerName


type alias WaitingRoom =
    { players : Players
    , cards : Cards
    }


type alias Game =
    { players : Players
    , turn : Id Player
    , stage : Stage
    }


type alias Players =
    Dict PlayerName Player


type alias PlayerName =
    String


type alias Player =
    { name : PlayerName
    , clientId : ClientId
    , score : Int
    , handId : Cards.HandId
    }


type Stage
    = ThinkingOfClue Cards
    | CollectingCards Cards.WithTable
    | GuessingCard Cards Table
    | Scoring
        { cards : Cards
        , table : Table
        , scores : Scores
        }


type alias Scores =
    {}


type PlayerJoinError
    = DuplicatePlayerName
    | NoWaitingRoom
    | CouldNotDealIn
