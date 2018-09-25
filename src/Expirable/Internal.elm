module Expirable.Internal exposing
    ( Remaining(..)
    , Total(..)
    , catMaybes
    , mapRemaining
    , mapTotal
    , oneSecondInMillis
    , percentComplete
    )


type Remaining a
    = Remaining a


type Total a
    = Total a


mapRemaining : (a -> b) -> Remaining a -> Remaining b
mapRemaining f (Remaining v) =
    Remaining <| f v


mapTotal : (a -> b) -> Total a -> Total b
mapTotal f (Total v) =
    Total <| f v


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.filterMap identity


oneSecondInMillis : Float
oneSecondInMillis =
    1000


percentComplete : Remaining Int -> Total Int -> Float
percentComplete (Remaining remaining) (Total total) =
    if remaining == total then
        0

    else
        1 - toFloat remaining / toFloat total
