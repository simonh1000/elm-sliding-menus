# Elm Sliding Menus

## Demo

## Installation

    elm-package install simonh1000/elm-sliding-menus

## Setup


import SlidingMenu


type alias Model =
    { mm : SlidingMenu.Model
    , userMessage : List String
    }


-- provide a message in your app to relay SlidingMenu messages
type Msg
    = MenuMsg SlidingMenu.Msg



myUpdateConfig : SlidingMenu.UpdateConfig
myUpdateConfig =
    { menu = menu
    , easing = Nothing -- use the default easing
    }

-- Handle the 3-tuple return value from SlidingMenu.update
update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MenuMsg msg ->
            let
                ( mm, cmd, maybeList ) =
                    SlidingMenu.update myUpdateConfig msg model.mm

                newModel =
                    { model
                        | mm = mm
                        , userMessage =
                            maybeList |> Maybe.withDefault model.userMessage
                    }
            in
                ( newModel, Cmd.map MenuMsg cmd )
