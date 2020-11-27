module Repo exposing (Id, Repo, empty, get, insert)

import Dict exposing (Dict)


{-| Use `repoOf` phantom type to mark the type of entity
contained in the Repo this Id relates to
-}
type Id entity
    = Id Int


type Repo entity
    = Repo (Dict Int entity)


empty : Repo a
empty =
    Repo Dict.empty


get : Id a -> Repo a -> Maybe a
get (Id id) (Repo repo) =
    Dict.get id repo


insert : a -> Repo a -> ( Id a, Repo a )
insert x (Repo repo) =
    let
        newId =
            Dict.size repo
    in
    ( Id newId
    , Repo <| Dict.insert newId x repo
    )
