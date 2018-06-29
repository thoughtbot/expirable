module Expirable
    exposing
        ( Expirable
        , SecondsRemaining(SecondsRemaining)
        , build
        , subscription
        , percentComplete
        , tickAll
        , value
        )

import Time


type SecondsRemaining
    = SecondsRemaining Time.Time


type SecondsTotal
    = SecondsTotal Time.Time


type alias LastTicked =
    Maybe Time.Time


type Expirable a
    = Expirable a SecondsRemaining SecondsTotal LastTicked



{- Constructor to convert a value to be expirable. It requires both the time
   duration (in seconds) that the value should remain and the value itself.


       toastWelcome : Expirable String
       toastWelcome =
           Expirable.build
               (Expirable.SecondsRemaining 15)
               "Welcome to the site!"
-}


build : SecondsRemaining -> a -> Expirable a
build (SecondsRemaining total) value =
    Expirable value (SecondsRemaining total) (SecondsTotal total) Nothing



{- Generate a subscription to tag an application-specific `Msg`; this is used
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



{- Within pattern-matching `Msg` in `update`, `tickAll` should be used to
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



{- Extract the value from the `Expirable a`. -}


value : Expirable a -> a
value (Expirable a _ _ _) =
    a



{- Calculate the percentage complete as a `Float` with a value between `0` and
   `1`.

   e.g. If a value is set to expire in 60 seconds and 15 seconds have passed,
   this would return `0.25`.
-}


percentComplete : Expirable a -> Float
percentComplete (Expirable _ (SecondsRemaining remaining) (SecondsTotal total) _) =
    remaining / total
