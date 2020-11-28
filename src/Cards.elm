module Cards exposing
    ( Card
    , CardError(..)
    , CardList
    , Cards
    , HandId
    , Table
    , WithTable
    , clearTable
    , dealIn
    , getHand
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
import Repo exposing (Repo)


type Card
    = Card String


type alias HandId =
    Repo.Id CardList


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


type alias CardsContents =
    { deck : CardList
    , discards : CardList
    , hands : Repo CardList
    , seed : Random.Seed
    }


new : Random.Seed -> Cards
new seed =
    let
        ( deck, newSeed ) =
            shuffleNewDeck seed
    in
    Cards
        { deck = deck
        , discards = []
        , hands = Repo.empty
        , seed = newSeed
        }


sizeOfHand : Int
sizeOfHand =
    7


show : Card -> String
show (Card name) =
    name


getHand : HandId -> Cards -> Maybe CardList
getHand id (Cards { hands }) =
    Repo.get id hands


getTable : WithTable -> Table
getTable (WithTable table _) =
    table


play : Card -> WithTable -> HandId -> CardResult
play card (WithTable table cards) id =
    getHand id cards
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


dealIn : Cards -> ( HandId, Cards )
dealIn ((Cards ({ deck, hands } as contents)) as cards) =
    let
        newHand =
            List.take sizeOfHand deck

        ( newId, newHands ) =
            Repo.add newHand hands
    in
    { contents
        | hands = newHands
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


deal : HandId -> CardList -> Cards -> Maybe Cards
deal id hand (Cards ({ deck, hands } as contents)) =
    case deck of
        [] ->
            Nothing

        card :: restOfDeck ->
            { contents
                | hands = Repo.insert id (card :: hand) hands
                , deck = restOfDeck
            }
                |> Cards
                |> shuffleIfNecessary
                |> Just


shuffleIfNecessary : Cards -> Cards
shuffleIfNecessary ((Cards { deck }) as cards) =
    if List.length deck < sizeOfHand then
        shuffleDiscardsIntoDeck cards

    else
        cards


shuffleDiscardsIntoDeck : Cards -> Cards
shuffleDiscardsIntoDeck (Cards ({ deck, discards, hands, seed } as cards)) =
    let
        discardCutoff =
            Repo.size hands * 2

        -- don't include recent discards in the shuffle, to avoid dealing them again too soon
        ( recentDiscards, laterDiscards ) =
            ( List.take discardCutoff discards, List.drop discardCutoff discards )

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


shuffleNewDeck : Random.Seed -> ( CardList, Random.Seed )
shuffleNewDeck seed =
    List.range 1 100
        |> List.map (String.fromInt >> String.padLeft 5 '0' >> Card)
        |> Random.List.shuffle
        |> flip Random.step seed
