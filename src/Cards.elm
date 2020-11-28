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
    Result CardError Cards


type alias WithTableResult =
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


startTable : Cards -> WithTable
startTable =
    WithTable []


clearTable : WithTable -> Cards
clearTable (WithTable table (Cards contents)) =
    Cards { contents | discards = table ++ contents.discards }


play : Card -> WithTable -> HandId -> WithTableResult
play card (WithTable table ((Cards contents) as cards)) id =
    getHand id cards
        |> Result.fromMaybe HandNotFound
        |> Result.andThen
            (\hand ->
                let
                    handsMinusCard =
                        Repo.update id (List.filter ((/=) card)) contents.hands
                in
                if not <| List.member card hand then
                    Err CardNotFound

                else
                    { contents | hands = handsMinusCard }
                        |> Cards
                        |> deal id
                        |> Result.map (WithTable <| card :: table)
            )


dealIn : Cards -> Maybe ( HandId, Cards )
dealIn ((Cards ({ deck, hands } as contents)) as cards) =
    let
        ( newHand, newDeck ) =
            ( List.take sizeOfHand deck, List.drop sizeOfHand contents.deck )

        ( newId, newHands ) =
            Repo.add newHand hands
    in
    if List.length newHand /= sizeOfHand then
        Nothing

    else
        { contents
            | hands = newHands
            , deck = newDeck
        }
            |> Cards
            |> shuffleIfNecessary
            |> Tuple.pair newId
            |> Just


deal : HandId -> Cards -> CardResult
deal id (Cards ({ deck, hands } as contents)) =
    case ( Repo.get id hands, deck ) of
        ( Nothing, _ ) ->
            Err HandNotFound

        ( _, [] ) ->
            Err NoCardsInDeck

        ( Just hand, card :: restOfDeck ) ->
            { contents
                | hands = Repo.replace id (card :: hand) hands
                , deck = restOfDeck
            }
                |> Cards
                |> shuffleIfNecessary
                |> Ok


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
