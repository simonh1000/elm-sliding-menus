module Main exposing (main)

import Html
import App exposing (..)
import Animation


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.previous, model.current, model.next ]
