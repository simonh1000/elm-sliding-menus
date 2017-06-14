module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List as L
import Animation exposing (..)


type alias Model =
    { path : List String
    , previous : Animation.State
    , current : Animation.State
    , next : Animation.State
    }


init : ( Model, Cmd Msg )
init =
    ( { path = []
      , previous =
            mkLeft -100.0
      , current =
            mkLeft 0.0
      , next =
            mkLeft 100.0
      }
    , Cmd.none
    )


mkLeft x =
    Animation.style
        [ Animation.left (percent x) ]


type MenuItem
    = Node String (List MenuItem)
    | Leaf String


type alias Menu =
    List MenuItem


menu =
    [ Node "l1" [ Leaf "l1 l1" ]
    , Leaf "l2"
    ]



-- UPDATE


type Msg
    = MoveRight
    | MoveLeft
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MoveLeft ->
            let
                current =
                    mkInterrupt -100.0 model.current

                next =
                    mkInterrupt 0.0 model.next
            in
                ( { model | current = current, next = next }, Cmd.none )

        MoveRight ->
            let
                current =
                    mkInterrupt 0.0 model.current

                next =
                    mkInterrupt 100.0 model.next
            in
                ( { model | current = current, next = next }, Cmd.none )

        Animate animMsg ->
            ( { model
                | current = Animation.update animMsg model.current
                , next = Animation.update animMsg model.next
              }
            , Cmd.none
            )


mkInterrupt x stateAnimation =
    Animation.interrupt [ Animation.toWith myEasing [ Animation.left (percent x) ] ] stateAnimation


myEasing =
    easing { duration = 2000, ease = identity }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "menu" ]
        , div [ class "menu-container" ]
            [ div (class "filler" :: Animation.render model.previous) [ text "previous" ]
            , ul (Animation.render model.current) <| viewLayer menu
            , div (class "filler" :: Animation.render model.next)
                [ span [ onClick MoveRight ] [ text "-->" ] ]
            ]
        ]


viewLayer : List MenuItem -> List (Html Msg)
viewLayer lst =
    lst
        |> L.map viewMenuItem


viewMenuItem item =
    case item of
        Node name _ ->
            viewNode name

        Leaf name ->
            li [] [ text name ]


viewNode name =
    li [ class "flex-h spread", onClick MoveLeft ]
        [ text name
        , matIcon "keyboard_arrow_right"
        ]



--


getName : MenuItem -> String
getName item =
    case item of
        Node name _ ->
            name

        Leaf name ->
            name


matIcon : String -> Html msg
matIcon name =
    i [ class "material-icons" ] [ text name ]


matIcon2 : msg -> String -> Html msg
matIcon2 msg name =
    i
        [ class "material-icons"
        , onClick msg
        ]
        [ text name ]



-- current =
--     Animation.interrupt
--         [ Animation.toWith myEasing
--             [ Animation.translate (percent -100.0) (percent 0.0) ]
--         ]
--         model.current
--
-- next =
--     Animation.interrupt
--         [ Animation.toWith myEasing
--             [ Animation.translate (percent 0.0) (percent 0.0) ]
--         ]
--         model.next
