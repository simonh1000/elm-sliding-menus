module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List as L
import Animation exposing (..)
import Animation.Messenger
import Animation.Spring.Presets exposing (..)


type MenuChoice
    = MenuChoice (List Int) (Maybe Int) Int (Maybe Int)


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
      , pages = MenuChoice [] Nothing 0 Nothing
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
    | SwitchDown
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MoveUp ->
            ( model, Cmd.none )

        -- let
        --     current =
        --         animateCurrent -100.0 model.current
        --
        --     next =
        --         queueAnimation 0.0 model.next
        --
        --     pages =
        --         L.take (L.length model.pages - 1) model.pages
        -- in
        --     ( { model | pages = pages, current = current, next = next }, Cmd.none )
        MoveDown ->
            let
                current =
                    animateCurrent -100.0 model.current

                next =
                    queueAnimation 0.0 model.next

                pages =
                    case model.pages of
                        MenuChoice ps mbp curr _ ->
                            MenuChoice ps mbp curr (Just <| curr + 1)
            in
                ( { model | pages = pages, current = current, next = next }, Cmd.none )

        SwitchDown ->
            -- Fired after transition down menu
            let
                _ =
                    Debug.log "SwitchDown" "****"

                pages =
                    case model.pages of
                        MenuChoice ps mbp curr (Just nxt) ->
                            MenuChoice
                                (ps ++ (mbp |> Maybe.map L.singleton |> Maybe.withDefault []))
                                (Just curr)
                                nxt
                                Nothing

                        _ ->
                            model.pages
            in
                ( { model | pages = pages } |> reset, Cmd.none )

        Animate animMsg ->
            let
                ( next, cmd ) =
                    Animation.Messenger.update animMsg model.next
            in
                ( { model
                    | current = Animation.update animMsg model.current
                    , next = next
                  }
                , cmd
                )


animateCurrent x stateAnimation =
    Animation.queue [ Animation.toWith myEasing [ Animation.left (percent x) ] ] stateAnimation


queueAnimation : Float -> Animation.Messenger.State Msg -> Animation.Messenger.State Msg
queueAnimation x state =
    Animation.queue
        [ Animation.toWith myEasing [ Animation.left (percent x) ]
        , Animation.Messenger.send SwitchDown
        ]
        state


mySpring =
    Animation.spring zippy


myEasing =
    easing { duration = 1000, ease = identity }


getCurrent mc =
    case mc of
        MenuChoice _ _ curr _ ->
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
        MenuChoice _ mbp curr mbn ->
            let
                prev =
                    mbp |> Maybe.map (mkPage model.previous) |> Maybe.withDefault (text "")

                next =
                    mbn |> Maybe.map (mkPage model.next) |> Maybe.withDefault (text "")
            in
                [ prev
                , mkPage model.current curr
                , next
                ]


mkPage st idx =
    div (class "page" :: Animation.render st)
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
