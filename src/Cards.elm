module Cards exposing
    ( Card
    , CardError(..)
    , Cards
    , DealError(..)
    , SearchError(..)
    , clearTable
    , dealIn
    , getList
    , getTable
    , play
    , show
    )

import Dict exposing (Dict)


type Card
    = Card String


type alias CardList =
    List Card


type Cards
    = Cards CardsContents


type DealError
    = NoCardsInDeck
    | NeedsShuffling Cards


type SearchError
    = HandNotFound
    | CardNotFound


type CardError
    = SearchError SearchError
    | DealError DealError


type alias CardResult =
    Result CardError Cards


type alias DealResult =
    Result DealError Cards


type alias Id =
    Int


type alias CardsContents =
    { deck : CardList
    , discards : CardList
    , table : CardList
    , hands : Dict Id CardList
    }


sizeOfHand =
    7


show : Card -> String
show (Card name) =
    name


getList : Id -> Cards -> Maybe CardList
getList id (Cards { hands }) =
    Dict.get id hands


getTable : Cards -> CardList
getTable (Cards { table }) =
    table


play : Card -> Cards -> Id -> CardResult
play card ((Cards contents) as cards) id =
    getList id cards
        |> Result.fromMaybe (SearchError HandNotFound)
        |> Result.andThen
            (\hand ->
                let
                    handMinusCard =
                        List.filter ((/=) card) hand
                in
                if hand == handMinusCard then
                    Err <| SearchError CardNotFound

                else
                    Cards { contents | table = card :: contents.table }
                        |> deal id handMinusCard
                        |> Result.mapError DealError
            )


dealIn : Cards -> ( Id, DealResult )
dealIn ((Cards contents) as cards) =
    let
        newId =
            makeId cards

        newHand =
            List.take sizeOfHand contents.deck
    in
    { contents
        | hands = Dict.insert newId newHand contents.hands
        , deck = List.drop sizeOfHand contents.deck
    }
        |> Cards
        |> needShuffling
        |> Tuple.pair newId


clearTable : Cards -> Cards
clearTable (Cards contents) =
    Cards
        { contents
            | table = []
            , discards = contents.table ++ contents.discards
        }


deal : Id -> CardList -> Cards -> DealResult
deal id hand (Cards contents) =
    case contents.deck of
        [] ->
            Err NoCardsInDeck

        card :: restOfDeck ->
            { contents
                | hands = Dict.insert id (card :: hand) contents.hands
                , deck = restOfDeck
            }
                |> Cards
                |> needShuffling


needShuffling : Cards -> DealResult
needShuffling ((Cards { deck }) as cards) =
    if List.length deck < sizeOfHand then
        Err <| NeedsShuffling cards

    else
        Ok cards


makeId : Cards -> Id
makeId (Cards contents) =
    Dict.keys contents.hands
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0
