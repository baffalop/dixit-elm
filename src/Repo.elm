module Repo exposing
    ( Id
    , Repo
    , add
    , empty
    , get
    , replace
    , size
    , update
    )

import Basics.Extra exposing (flip)
import Dict exposing (Dict)


type alias Key =
    Int


type Id entity
    = Id Key


type Repo entity
    = Repo (Dict Key entity)


empty : Repo a
empty =
    Repo Dict.empty


get : Id a -> Repo a -> Maybe a
get id (Repo repo) =
    Dict.get (toKey id) repo


add : a -> Repo a -> ( Id a, Repo a )
add x repo =
    let
        dict =
            toDict repo

        newId =
            Dict.size dict
    in
    ( Id newId
    , Repo <| Dict.insert newId x dict
    )


replace : Id a -> a -> Repo a -> Repo a
replace id x repo =
    Dict.update
        (toKey id)
        (Maybe.map <| always x)
        (toDict repo)
        |> Repo


update : Id a -> (a -> a) -> Repo a -> Repo a
update id f repo =
    Dict.update
        (toKey id)
        (Maybe.map f)
        (toDict repo)
        |> Repo


size : Repo a -> Int
size =
    toDict >> Dict.size


toKey : Id a -> Key
toKey (Id key) =
    key


toDict : Repo a -> Dict Key a
toDict (Repo dict) =
    dict
