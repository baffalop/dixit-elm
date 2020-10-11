module Cards exposing
    ( Card
    , CardError(..)
    , Cards
    , DealError(..)
    , SearchError(..)
    , WithTable
    , clearTable
    , dealIn
    , getList
    , getTable
    , play
    , show
    , startTable
    )

import Dict exposing (Dict)


type Card
    = Card String


type alias CardList =
    List Card


type Cards
    = Cards CardsContents


type alias Table =
    CardList


type WithTable
    = WithTable Table Cards


type DealError cards
    = NoCardsInDeck
    | NeedsShuffling cards


type SearchError
    = HandNotFound
    | CardNotFound


type CardError cards
    = SearchError SearchError
    | DealError (DealError cards)


type alias CardResult =
    Result (CardError Cards) Cards


type alias TableResult =
    Result (CardError WithTable) WithTable


type alias DealResult =
    Result (DealError Cards) Cards


type alias Id =
    Int


type alias CardsContents =
    { deck : CardList
    , discards : CardList
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


getTable : WithTable -> Table
getTable (WithTable table _) =
    table


play : Card -> WithTable -> Id -> TableResult
play card (WithTable table cards) id =
    getList id cards
        |> Result.fromMaybe (SearchError HandNotFound)
        |> Result.andThen
            (\hand ->
                let
                    handMinusCard =
                        List.filter ((/=) card) hand

                    withTable =
                        WithTable <| card :: table
                in
                if hand == handMinusCard then
                    Err <| SearchError CardNotFound

                else
                    cards
                        |> deal id handMinusCard
                        |> Result.mapError (mapDealError withTable >> DealError)
                        |> Result.map withTable
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


startTable : Cards -> WithTable
startTable =
    WithTable []


clearTable : WithTable -> Cards
clearTable (WithTable table (Cards contents)) =
    Cards { contents | discards = table ++ contents.discards }


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


mapDealError : (a -> b) -> DealError a -> DealError b
mapDealError mapper error =
    case error of
        NoCardsInDeck ->
            NoCardsInDeck

        NeedsShuffling contents ->
            NeedsShuffling <| mapper contents
