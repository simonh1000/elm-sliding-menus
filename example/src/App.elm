module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Animation exposing (..)
import Animation.Spring.Presets exposing (..)
import MobileMenu as MM exposing (MenuItem, leaf)


type alias Model =
    { mm : MM.Model
    , userMessage : List String
    }


init : Model
init =
    { mm = MM.init
    , userMessage = []
    }


type Msg
    = MenuMsg MM.Msg
    | OnLeafClick (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MenuMsg msg ->
            let
                ( mm, c1, maybeMsg ) =
                    MM.update myUpdateConfig msg model.mm

                ( newModel, c2 ) =
                    case maybeMsg of
                        Just msg_ ->
                            update msg_ { model | mm = mm }

                        Nothing ->
                            ( { model | mm = mm }, Cmd.none )
            in
                ( newModel
                , Cmd.batch [ Cmd.map MenuMsg c1, c2 ]
                )

        OnLeafClick str ->
            ( { model | userMessage = str }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "menu" ]
        , MM.view myViewConfig model.mm |> Html.map MenuMsg
        , div [] [ text <| toString model.userMessage ]
        ]



--


myViewConfig : MM.ViewConfig
myViewConfig =
    { menu = menu
    }


myUpdateConfig : MM.UpdateConfig Msg
myUpdateConfig =
    { menu = menu
    , easing = mySpring
    , onLeafClick = OnLeafClick
    }


mySpring : Animation.Interpolation
mySpring =
    Animation.spring zippy


myEasing : Animation.Interpolation
myEasing =
    easing { duration = 1000, ease = identity }


categories : List MenuItem
categories =
    [ leaf "Food", leaf "Hotels", leaf "Bars" ]


menu : MM.MenuItem
menu =
    MM.node "root"
        [ MM.node "Belgium"
            [ MM.node "Brussels" categories
            , MM.node "Gent" categories
            , MM.node "Antwerp" categories
            , MM.node "Liege" categories
            ]
        , MM.node "France"
            [ MM.node "Paris" categories
            , MM.node "Marseille" categories
            , MM.node "Bordeaux" categories
            , MM.node "Lille" categories
            ]
        , MM.node "UK"
            [ MM.node "Lodon" categories
            , MM.node "Manchester" categories
            , MM.node "Glasgow" categories
            , MM.node "Bristol" categories
            ]
        ]
