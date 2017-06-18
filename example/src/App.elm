module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import SlidingMenu


type alias Model =
    { mm : SlidingMenu.Model
    , userMessage : List String
    }


init : Model
init =
    { mm = SlidingMenu.init
    , userMessage = []
    }



--


type Msg
    = MenuMsg SlidingMenu.Msg


myUpdateConfig : SlidingMenu.UpdateConfig
myUpdateConfig =
    { menu = menu
    , easing = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MenuMsg msg ->
            let
                ( mm, c1, maybeList ) =
                    SlidingMenu.update myUpdateConfig msg model.mm

                newModel =
                    { model | mm = mm, userMessage = maybeList |> Maybe.withDefault model.userMessage }
            in
                ( newModel, Cmd.map MenuMsg c1 )



--


myViewConfig : SlidingMenu.ViewConfig
myViewConfig =
    { menu = menu
    , back = "Back"
    }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "menu" ]
        , SlidingMenu.view myViewConfig model.mm |> Html.map MenuMsg
        , div [] [ text <| toString model.userMessage ]
        ]



--


menu : SlidingMenu.MenuItem
menu =
    SlidingMenu.node "root"
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
