module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List as L
import Animation exposing (..)
import Animation.Messenger
import Animation.Spring.Presets exposing (..)


type
    MenuChoice
    -- List Int in reverse order
    = MenuChoice (List Int) Int (Maybe Int)


type alias Model =
    { path : List String
    , pages : MenuChoice
    , previous : Animation.Messenger.State Msg
    , current : Animation.Messenger.State Msg
    , next : Animation.Messenger.State Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { path = []
      , pages = MenuChoice [] 0 Nothing
      , previous =
            mkLeft -100.0
      , current =
            mkLeft 0.0
      , next =
            mkLeft 100.0
      }
    , Cmd.none
    )


reset : Model -> Model
reset model =
    { model
        | previous =
            mkLeft -100.0
        , current =
            mkLeft 0.0
        , next =
            mkLeft 100.0
    }


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
    = MoveUp
    | MoveDown
    | SwitchUp
    | SwitchDown
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MoveUp ->
            let
                previous =
                    queueWithMsg SwitchUp 0.0 model.previous

                current =
                    animateCurrent 100.0 model.current
            in
                ( { model | previous = previous, current = current }, Cmd.none )

        MoveDown ->
            let
                current =
                    animateCurrent -100.0 model.current

                next =
                    queueWithMsg SwitchDown 0.0 model.next

                pages =
                    case model.pages of
                        MenuChoice ps curr _ ->
                            MenuChoice ps curr (Just <| curr + 1)
            in
                ( { model | pages = pages, current = current, next = next }, Cmd.none )

        SwitchDown ->
            -- Fired after transition down menu
            let
                pages =
                    case model.pages of
                        MenuChoice ps curr (Just nxt) ->
                            MenuChoice (curr :: ps) nxt Nothing

                        _ ->
                            model.pages
            in
                ( { model | pages = pages } |> reset, Cmd.none )

        SwitchUp ->
            -- Fired after transition up menu
            let
                pages =
                    case model.pages of
                        MenuChoice (p :: ps) _ _ ->
                            MenuChoice ps p Nothing

                        _ ->
                            Debug.log "Unexpected error MoveUp" model.pages
            in
                ( { model | pages = pages } |> reset, Cmd.none )

        Animate animMsg ->
            let
                ( next, cmdNext ) =
                    Animation.Messenger.update animMsg model.next

                ( previous, cmdPrev ) =
                    Animation.Messenger.update animMsg model.previous
            in
                ( { model
                    | previous = previous
                    , current = Animation.update animMsg model.current
                    , next = next
                  }
                , Cmd.batch [ cmdNext, cmdPrev ]
                )


animateCurrent x stateAnimation =
    Animation.queue [ Animation.toWith myEasing [ Animation.left (percent x) ] ] stateAnimation


queueWithMsg : Msg -> Float -> Animation.Messenger.State Msg -> Animation.Messenger.State Msg
queueWithMsg msg x state =
    Animation.queue
        [ Animation.toWith myEasing [ Animation.left (percent x) ]
        , Animation.Messenger.send msg
        ]
        state


mySpring =
    Animation.spring zippy


myEasing =
    easing { duration = 1000, ease = identity }


getCurrent mc =
    case mc of
        MenuChoice _ curr _ ->
            curr



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "menu" ]
        , div [ class "menu-container" ]
            (viewPages model)

        -- [ div (class "filler" :: Animation.render model.previous) [ text "previous" ]
        -- , ul (Animation.render model.current) <| viewLayer menu
        -- , div (class "filler" :: Animation.render model.next)
        --     [ span [ onClick MoveRight ] [ text "-->" ] ]
        -- ]
        ]


viewPages model =
    case model.pages of
        MenuChoice (p :: _) curr mbn ->
            [ mkPage "prev" model.previous p
            , mkPage "curr" model.current curr
            , mbn |> Maybe.map (mkPage "next" model.next) |> Maybe.withDefault (text "")
            ]

        MenuChoice [] curr mbn ->
            [ mkPage "curr" model.current curr
            , mbn |> Maybe.map (mkPage "next" model.next) |> Maybe.withDefault (text "")
            ]


mkPage tag_ st idx =
    div (class ("page " ++ tag_) :: Animation.render st)
        [ if idx == 0 then
            text ""
          else
            button [ onClick MoveUp ] [ text "Up" ]
        , text <| toString idx
        , button [ onClick MoveDown ] [ text "Deeper" ]
        ]



-- viewLayer : List MenuItem -> List (Html Msg)
-- viewLayer lst =
--     lst
--         |> L.map viewMenuItem
--
--
-- viewMenuItem item =
--     case item of
--         Node name _ ->
--             viewNode name
--
--         Leaf name ->
--             li [] [ text name ]
--
--
-- viewNode name =
--     li [ class "flex-h spread", onClick MoveLeft ]
--         [ text name
--         , matIcon "keyboard_arrow_right"
--         ]
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
