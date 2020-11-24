module Id exposing (Id, IdPool, initPool, new)


type Id
    = Id Int


type IdPool
    = IdPool Int


initPool : IdPool
initPool =
    IdPool 0


new : IdPool -> ( Id, IdPool )
new (IdPool maxId) =
    let
        newId =
            maxId + 1
    in
    ( Id newId, IdPool newId )
