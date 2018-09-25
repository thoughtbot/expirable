module Expirable exposing
    ( Expirable, build, seconds
    , subscription, tickAll
    , value, percentComplete
    )

{-| The concept of a value that expires or disappears over a period of time is
relatively common in applications. The `Expirable a` type allows one to wrap a
value as one that expires after a certain period of seconds.

It's expected that a `List (Expirable a)` be used to manage a list of things
that can expire; with this, the `tickAll` function can be used to take in the
list of `Expirable a` and return a new list of `Expirable a`. This function
handles removal of items that have expired.

This is a portion of an application that manages a list of toast messages.
These are displayed for a period of time and are then hidden.

Without digging into render the messages, we display the corresponding `Msg`,
subscriptions, and modeling to manage the list of toast messages.

    import Expirable exposing (Expirable)

    type alias Model =
        { toastMessages : List (Expirable String)
        }

    type Msg
        = DecrementToastMessages Time.Posix

    initial : Model
    initial =
        { toastMessages =
            [ Expirable.build (Expirable.seconds 5) "Hi there"
            , Expirable.build (Expirable.seconds 30) "This goes longer"
            ]
        }

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Expirable.subscription DecrementToastMessages

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            DecrementToastMessages time ->
                ( { model
                    | toastMessages = Expirable.tickAll time model.toastMessages
                  }
                , Cmd.none
                )


# Type and Constructors

@docs Expirable, build, seconds


# Subscriptions and Management

@docs subscription, tickAll


# Information

@docs value, percentComplete

-}

import Expirable.Internal exposing (..)
import Expirable.Seconds as Seconds exposing (Seconds(..))
import Time


{-| The core `Expirable` type describing a value that can expire in the future.
-}
type Expirable a
    = Expirable
        { expirableValue : a
        , remaining : Remaining Seconds
        , total : Total Seconds
        , lastTicked : Maybe Time.Posix
        }


{-| Constructor to convert a value to be expirable. It requires both the time
duration (in seconds) that the value should remain and the value itself.

       toastWelcome : Expirable String
       toastWelcome =
           Expirable.build
               (Expirable.seconds 15)
               "Welcome to the site!"

-}
build : Remaining Seconds -> a -> Expirable a
build (Remaining time) a =
    Expirable
        { expirableValue = a
        , remaining = Remaining time
        , total = Total time
        , lastTicked = Nothing
        }


{-| Generate a subscription to tag an application-specific `Msg`; this is used
in conjunction with `tickAll` to manage a list of `Expirable a`.

        subscriptions : Model -> Sub Msg
        subscriptions model =
            Expirable.subscription DecrementToastMessages

-}
subscription : (Time.Posix -> a) -> Sub a
subscription =
    Time.every oneSecondInMillis


anySecondsRemaining : Remaining Seconds -> Bool
anySecondsRemaining (Remaining remaining) =
    Seconds.toInt remaining > 0


{-| Within pattern-matching `Msg` in `update`, `tickAll` should be used to
manage a list of `Expirable a` in order to expire the appropriate set of values.

        type Msg
            = DecrementToastMessages Time.Posix


        update : Msg -> Model -> ( Model, Cmd Msg )
        update msg model =
            case msg of
                DecrementToastMessages time ->
                    { model
                        | toastMessages = Expirable.tickAll time model.toastMessages
                    }
                        ! []

-}
tickAll : Time.Posix -> List (Expirable a) -> List (Expirable a)
tickAll time =
    catMaybes << List.map (tick time)


tick : Time.Posix -> Expirable a -> Maybe (Expirable a)
tick time (Expirable ({ expirableValue, remaining, lastTicked } as record)) =
    let
        newSecondsRemaining =
            mapRemaining (Seconds.subtract secondToDecrease) remaining

        secondToDecrease =
            Seconds.seconds <|
                case lastTicked of
                    Nothing ->
                        1

                    Just oldTime ->
                        round <| toFloat (Time.posixToMillis time - Time.posixToMillis oldTime) / oneSecondInMillis
    in
    if anySecondsRemaining newSecondsRemaining then
        Just <|
            Expirable
                { record
                    | remaining = newSecondsRemaining
                    , lastTicked = Just time
                }

    else
        Nothing


{-| Extract the value from the `Expirable a`.
-}
value : Expirable a -> a
value (Expirable { expirableValue }) =
    expirableValue


{-| Calculate the percentage complete as a `Float` with a value between `0` and
`1`.

e.g. If a value is set to expire in 60 seconds and 15 seconds have passed,
this would return `0.25`.

-}
percentComplete : Expirable a -> Float
percentComplete (Expirable { remaining, total }) =
    Expirable.Internal.percentComplete
        (mapRemaining Seconds.toInt remaining)
        (mapTotal Seconds.toInt total)


{-| Construct a value representing the number of seconds before a value expires.
-}
seconds : Int -> Remaining Seconds
seconds =
    Remaining << Seconds.seconds
