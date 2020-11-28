module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , FrontendModel
    , FrontendMsg(..)
    , ToBackend(..)
    , ToFrontend(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Cards exposing (Cards, Table)
import Dict exposing (Dict)
import Repo exposing (Id, Repo)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    }


type alias BackendModel =
    { games : Repo Game
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend


type Game
    = GameNotStarted
        { players : Players
        , cards : Cards
        }
    | GameInProgress GameState


type alias GameState =
    { players : Players
    , turn : PlayerName
    , stage : Stage
    }


type alias Players =
    Repo Player


type alias PlayerName =
    String


type alias Player =
    { name : PlayerName
    , score : Int
    , handId : Cards.Id
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
