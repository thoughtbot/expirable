# Expirable

Manage values that expire after a period of time.

## Example

```elm
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
```

## License

See the [LICENSE](/LICENSE) file.

## About thoughtbot

![thoughtbot](http://presskit.thoughtbot.com/images/thoughtbot-logo-for-readmes.svg)

Expirable is maintained and funded by thoughtbot, inc.
The names and logos for thoughtbot are trademarks of thoughtbot, inc.

We love open source software!
See [our other projects][community] or
[hire us][hire] to design, develop, and grow your product.

[community]: https://thoughtbot.com/community?utm_source=github
[hire]: https://thoughtbot.com/hire-us?utm_source=github
