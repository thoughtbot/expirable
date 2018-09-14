module Tests exposing (suite)

import Expect exposing (Expectation)
import Expirable
import Fuzz
import Test exposing (..)
import Time


suite : Test
suite =
    describe "The Expirable module"
        [ fuzz positiveInt "always returns zero progress if the expirable hasn't been ticked" <|
            \expiresInSeconds ->
                Expect.equal
                    (Expirable.percentComplete <| expiresInNSeconds expiresInSeconds)
                    0
        , fuzz positiveInt "handles tickAll independent of time start" <|
            \start ->
                let
                    startSeconds =
                        seconds start
                in
                Expect.equal
                    ([ expiresInNSeconds 2 ]
                        |> Expirable.tickAll (secondsToTime startSeconds)
                        |> List.map Expirable.percentComplete
                    )
                    [ 0.5 ]
        , fuzz positiveInt "removes expirables that run out of time with tickAll" <|
            \start ->
                let
                    startSeconds =
                        seconds start

                    secondTickSeconds =
                        addSeconds startSeconds (seconds 1)
                in
                Expect.equal
                    ([ expiresInNSeconds 2 ]
                        |> Expirable.tickAll (secondsToTime startSeconds)
                        |> Expirable.tickAll (secondsToTime secondTickSeconds)
                    )
                    []
        , fuzz positiveInt "retains the correct expirables with tickAll" <|
            \start ->
                let
                    startSeconds =
                        seconds start

                    secondTickSeconds =
                        addSeconds startSeconds (seconds 14)
                in
                Expect.equal
                    ([ expiresInNSeconds 10, expiresInNSeconds 30, expiresInNSeconds 60 ]
                        |> Expirable.tickAll (secondsToTime startSeconds)
                        |> Expirable.tickAll (secondsToTime secondTickSeconds)
                        |> List.map Expirable.percentComplete
                    )
                    [ 0.5, 0.25 ]
        , fuzzValueForType "calculates value for String" Fuzz.string
        , fuzzValueForType "calculates value for Bool" Fuzz.bool
        , fuzzValueForType "calculates value for Int" Fuzz.int
        ]


fuzzValueForType : String -> Fuzz.Fuzzer a -> Test
fuzzValueForType message typeFuzzer =
    fuzz typeFuzzer message <|
        \currentValue ->
            Expect.equal
                (Expirable.value <| Expirable.build (Expirable.seconds 1) currentValue)
                currentValue


type Seconds
    = Seconds Int


seconds : Int -> Seconds
seconds =
    Seconds


addSeconds : Seconds -> Seconds -> Seconds
addSeconds (Seconds a) (Seconds b) =
    Seconds <| a + b


secondsToTime : Seconds -> Time.Posix
secondsToTime (Seconds s) =
    Time.millisToPosix <| s * 1000


expiresInNSeconds : Int -> Expirable.Expirable String
expiresInNSeconds i =
    Expirable.build (Expirable.seconds i) "Welcome to the site!"


startTime : Time.Posix
startTime =
    secondsToTime <| seconds 0


positiveInt : Fuzz.Fuzzer Int
positiveInt =
    Fuzz.map Basics.abs Fuzz.int
