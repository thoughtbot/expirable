module Expirable
    exposing
        ( Expirable
        , seconds
        , build
        , subscription
        , percentComplete
        , tickAll
        , value
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
        = DecrementToastMessages Time.Time

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
                { model
                    | toastMessages = Expirable.tickAll time model.toastMessages
                }
                    ! []


# Type and Constructors

@docs Expirable, build, seconds


# Subscriptions and Management

@docs subscription, tickAll


# Information

@docs value, percentComplete

-}

import Time


type SecondsRemaining
    = SecondsRemaining Time.Time


type SecondsTotal
    = SecondsTotal Time.Time


type alias LastTicked =
    Maybe Time.Time


{-| The core `Expirable` type describing a value that can expire in the future.
-}
type Expirable a
    = Expirable a SecondsRemaining SecondsTotal LastTicked


{-| Constructor to convert a value to be expirable. It requires both the time
duration (in seconds) that the value should remain and the value itself.

       toastWelcome : Expirable String
       toastWelcome =
           Expirable.build
               (Expirable.seconds 15)
               "Welcome to the site!"

-}
build : SecondsRemaining -> a -> Expirable a
build (SecondsRemaining total) value =
    Expirable value (SecondsRemaining total) (SecondsTotal total) Nothing


{-| Generate a subscription to tag an application-specific `Msg`; this is used
in conjunction with `tickAll` to manage a list of `Expirable a`.

        subscriptions : Model -> Sub Msg
        subscriptions model =
            Expirable.subscription DecrementToastMessages

-}
subscription : (Time.Time -> a) -> Sub a
subscription =
    Time.every Time.second


anySecondsRemaining : SecondsRemaining -> Bool
anySecondsRemaining (SecondsRemaining i) =
    i > 0


{-| Within pattern-matching `Msg` in `update`, `tickAll` should be used to
manage a list of `Expirable a` in order to expire the appropriate set of values.

        type Msg
            = DecrementToastMessages Time.Time


        update : Msg -> Model -> ( Model, Cmd Msg )
        update msg model =
            case msg of
                DecrementToastMessages time ->
                    { model
                        | toastMessages = Expirable.tickAll time model.toastMessages
                    }
                        ! []

-}
tickAll : Time.Time -> List (Expirable a) -> List (Expirable a)
tickAll time =
    catMaybes << List.map (tick time)


decreaseSecondsRemaining : Float -> SecondsRemaining -> SecondsRemaining
decreaseSecondsRemaining i (SecondsRemaining remaining) =
    SecondsRemaining <| remaining - i


tick : Time.Time -> Expirable a -> Maybe (Expirable a)
tick time (Expirable a seconds secondsTotal lastTicked) =
    let
        newSecondsRemaining =
            decreaseSecondsRemaining secondToDecrease seconds

        secondToDecrease =
            toFloat <|
                case lastTicked of
                    Nothing ->
                        1

                    Just oldTime ->
                        round <| (time - oldTime) / 1000
    in
        if anySecondsRemaining newSecondsRemaining then
            Just <| Expirable a newSecondsRemaining secondsTotal (Just time)
        else
            Nothing


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.filterMap identity


{-| Extract the value from the `Expirable a`.
-}
value : Expirable a -> a
value (Expirable a _ _ _) =
    a


{-| Calculate the percentage complete as a `Float` with a value between `0` and
`1`.

e.g. If a value is set to expire in 60 seconds and 15 seconds have passed,
this would return `0.25`.

-}
percentComplete : Expirable a -> Float
percentComplete (Expirable _ (SecondsRemaining remaining) (SecondsTotal total) _) =
    remaining / total


{-| Construct a value representing the number of seconds before a value expires.
-}
seconds : Time.Time -> SecondsRemaining
seconds =
    SecondsRemaining
