module Main exposing (main)

import Html
import App exposing (..)
import SlidingMenu


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map MenuMsg (SlidingMenu.subscriptions model.menu)
