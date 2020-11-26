module Cards exposing
    ( Card
    , CardError(..)
    , CardList
    , Cards
    , Id
    , Table
    , WithTable
    , clearTable
    , dealIn
    , getList
    , getTable
    , new
    , play
    , show
    , startTable
    )

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Random
import Random.List


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


type CardError
    = HandNotFound
    | CardNotFound
    | NoCardsInDeck


type alias CardResult =
    Result CardError WithTable


type alias Id =
    Int


type alias CardsContents =
    { deck : CardList
    , discards : CardList
    , hands : Dict Id CardList
    , seed : Random.Seed
    }


new : Random.Seed -> Cards
new seed =
    let
        ( shuffledDeck, newSeed ) =
            shuffleNewDeck seed
    in
    Cards
        { deck = shuffledDeck
        , discards = []
        , hands = Dict.empty
        , seed = newSeed
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


play : Card -> WithTable -> Id -> CardResult
play card (WithTable table cards) id =
    getList id cards
        |> Result.fromMaybe HandNotFound
        |> Result.andThen
            (\hand ->
                let
                    handMinusCard =
                        List.filter ((/=) card) hand

                    withTable =
                        WithTable <| card :: table
                in
                if hand == handMinusCard then
                    Err CardNotFound

                else
                    cards
                        |> deal id handMinusCard
                        |> Result.fromMaybe NoCardsInDeck
                        |> Result.map withTable
            )


dealIn : Cards -> ( Id, Cards )
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
        |> shuffleIfNecessary
        |> Tuple.pair newId


startTable : Cards -> WithTable
startTable =
    WithTable []


clearTable : WithTable -> Cards
clearTable (WithTable table (Cards contents)) =
    Cards { contents | discards = table ++ contents.discards }


deal : Id -> CardList -> Cards -> Maybe Cards
deal id hand (Cards contents) =
    case contents.deck of
        [] ->
            Nothing

        card :: restOfDeck ->
            { contents
                | hands = Dict.insert id (card :: hand) contents.hands
                , deck = restOfDeck
            }
                |> Cards
                |> shuffleIfNecessary
                |> Just


shuffleIfNecessary : Cards -> Cards
shuffleIfNecessary ((Cards { deck }) as cards) =
    if List.length deck > sizeOfHand then
        shuffleDiscardsIntoDeck cards

    else
        cards


shuffleDiscardsIntoDeck : Cards -> Cards
shuffleDiscardsIntoDeck (Cards ({ deck, discards, seed } as cards)) =
    let
        -- don't include recent discards in the shuffle, to avoid dealing them again too soon
        ( recentDiscards, laterDiscards ) =
            ( List.take sizeOfHand discards, List.drop sizeOfHand discards )

        ( shuffled, newSeed ) =
            laterDiscards
                |> Random.List.shuffle
                |> flip Random.step seed
    in
    Cards
        { cards
            | discards = recentDiscards
            , deck = deck ++ shuffled
            , seed = newSeed
        }


makeId : Cards -> Id
makeId (Cards contents) =
    Dict.keys contents.hands
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0


shuffleNewDeck : Random.Seed -> ( CardList, Random.Seed )
shuffleNewDeck seed =
    List.range 1 100
        |> List.map (String.fromInt >> String.padLeft 5 '0' >> Card)
        |> Random.List.shuffle
        |> flip Random.step seed
