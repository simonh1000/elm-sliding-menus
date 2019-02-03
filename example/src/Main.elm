module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import SlidingMenu


type alias Model =
    { menu : SlidingMenu.Model
    , userMessage : List String
    }


init : Model
init =
    { menu = SlidingMenu.init
    , userMessage = []
    }



-- UPDATE


type Msg
    = MenuMsg SlidingMenu.Msg


myUpdateConfig : SlidingMenu.UpdateConfig
myUpdateConfig =
    { menu = menuData
    , easing = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case Debug.log "" message of
        MenuMsg msg ->
            let
                ( menu, cmds, maybeList ) =
                    SlidingMenu.update myUpdateConfig msg model.menu

                newModel =
                    { model
                        | menu = menu
                        , userMessage = maybeList |> Maybe.withDefault model.userMessage
                    }
            in
            ( newModel, Cmd.map MenuMsg cmds )



--


myViewConfig : SlidingMenu.ViewConfig
myViewConfig =
    { menu = menuData
    , back = "Back"
    }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Travel guide" ]
        , SlidingMenu.view myViewConfig model.menu |> Html.map MenuMsg
        , div [] [ text <| String.join ", " model.userMessage ]
        ]



--


menuData : List SlidingMenu.MenuItem
menuData =
    [ SlidingMenu.node "Belgium"
        [ SlidingMenu.node "Brussels" categories
        , SlidingMenu.node "Gent" categories
        , SlidingMenu.node "Antwerp" categories
        , SlidingMenu.node "Liege" categories
        ]
    , SlidingMenu.node "France"
        [ SlidingMenu.node "Paris" categories
        , SlidingMenu.node "Marseille" categories
        , SlidingMenu.node "Bordeaux" categories
        , SlidingMenu.node "Lille" categories
        ]
    , SlidingMenu.node "UK"
        [ SlidingMenu.node "London" categories
        , SlidingMenu.node "Manchester" categories
        , SlidingMenu.node "Glasgow" categories
        , SlidingMenu.node "Bristol" categories
        ]
    ]


categories : List SlidingMenu.MenuItem
categories =
    [ SlidingMenu.leaf "Food", SlidingMenu.leaf "Hotels", SlidingMenu.leaf "Bars" ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Int Model Msg
main =
    Browser.document
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view =
            \m ->
                { title = "elm-sliding-menu example"
                , body = [ view m ]
                }
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map MenuMsg (SlidingMenu.subscriptions model.menu)
