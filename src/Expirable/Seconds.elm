module Expirable.Seconds exposing
    ( Seconds
    , add
    , map
    , seconds
    , subtract
    , toInt
    , toPosix
    )

import Time


type Seconds
    = Seconds Int


seconds : Int -> Seconds
seconds =
    Seconds


map : (Int -> Int) -> Seconds -> Seconds
map f (Seconds i) =
    Seconds <| f i


add : Seconds -> Seconds -> Seconds
add (Seconds s1) (Seconds s2) =
    Seconds <| s1 + s2


subtract : Seconds -> Seconds -> Seconds
subtract (Seconds s1) (Seconds s2) =
    Seconds <| s2 - s1


toPosix : Seconds -> Time.Posix
toPosix (Seconds s) =
    Time.millisToPosix <| s * 1000


toInt : Seconds -> Int
toInt (Seconds s) =
    s
