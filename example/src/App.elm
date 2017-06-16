module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List as L
import Animation exposing (..)
import Animation.Messenger
import Animation.Spring.Presets exposing (..)


type
    MenuChoices a
    -- List Int in reverse order
    = MenuChoices (List a) a (Maybe a)


type alias Model =
    { pages : MenuChoices String
    , previous : Animation.Messenger.State Msg
    , current : Animation.Messenger.State Msg
    , next : Animation.Messenger.State Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { pages = MenuChoices [] "root" Nothing
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
    = Node String Menu


type alias Menu =
    List MenuItem



-- UPDATE


type Msg
    = MoveUp
    | MoveDown String
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

        MoveDown chosen ->
            let
                current =
                    animateCurrent -100.0 model.current

                next =
                    queueWithMsg SwitchDown 0.0 model.next

                pages =
                    case model.pages of
                        MenuChoices ps curr _ ->
                            MenuChoices ps curr (Just chosen)
            in
                ( { model | pages = pages, current = current, next = next }, Cmd.none )

        SwitchDown ->
            -- Fired after transition down menu
            let
                pages =
                    case model.pages of
                        MenuChoices ps curr (Just nxt) ->
                            MenuChoices (curr :: ps) nxt Nothing

                        _ ->
                            model.pages
            in
                ( { model | pages = pages } |> reset, Cmd.none )

        SwitchUp ->
            -- Fired after transition up menu
            let
                pages =
                    case model.pages of
                        MenuChoices (p :: ps) _ _ ->
                            MenuChoices ps p Nothing

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
        MenuChoices _ curr _ ->
            curr



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "menu" ]
        , div [ class "menu-container" ] (viewPages model)
        ]



-- Simple, with integers
-- viewPages model =
--     case model.pages of
--         MenuChoices (p :: _) curr mbn ->
--             [ mkPage "prev" model.previous p
--             , mkPage "curr" model.current curr
--             , mbn |> Maybe.map (mkPage "next" model.next) |> Maybe.withDefault (text "")
--             ]
--
--         MenuChoices [] curr mbn ->
--             [ mkPage "curr" model.current curr
--             , mbn |> Maybe.map (mkPage "next" model.next) |> Maybe.withDefault (text "")
--             ]
--
--
-- mkPage tag_ st idx =
--     div (class ("page " ++ tag_) :: Animation.render st)
--         [ if idx == 0 then
--             text ""
--           else
--             button [ onClick MoveUp ] [ text "Up" ]
--         , text <| toString idx
--         , button [ onClick MoveDown ] [ text "Deeper" ]
--         ]
--


viewPages : Model -> List (Html Msg)
viewPages model =
    case model.pages of
        MenuChoices (p :: _) curr mbn ->
            [ text "tbc" ]

        -- [ mkPage "prev" model.previous p
        -- , mkPage "curr" model.current curr
        -- , mbn |> Maybe.map (mkPage "next" model.next) |> Maybe.withDefault (text "")
        -- ]
        MenuChoices [] curr mbn ->
            [ mkCurrentPage model.current model.pages

            -- , mbn |> Maybe.map (mkPage "next" model.next) |> Maybe.withDefault (text "")
            ]



-- mkCurrentPage : MenuChoices -> Html MoveUp


mkCurrentPage st (MenuChoices ps curr _) =
    case getMenuAtPath (L.reverse <| curr :: ps) of
        Err err ->
            text err

        Ok lst ->
            div (class ("page current") :: Animation.render st) <|
                (if ps == [] then
                    text ""
                 else
                    button [ onClick MoveUp ] [ text "Up" ]
                )
                    :: L.map (\(Node item _) -> li [ onClick (MoveDown item) ] [ text item ]) lst


categories =
    [ Node "Food" [], Node "Hotels" [], Node "Bars" [] ]


menu : MenuItem
menu =
    Node "root"
        [ Node "Belgium"
            [ Node "Brussels" categories
            , Node "Gent" categories
            , Node "Antwerp" categories
            , Node "Liege" categories
            ]
        , Node "France"
            [ Node "Paris" categories
            , Node "Marseille" categories
            , Node "Bordeaux" categories
            , Node "Lille" categories
            ]
        , Node "UK"
            [ Node "Lodon" categories
            , Node "Manchester" categories
            , Node "Glasgow" categories
            , Node "Bristol" categories
            ]
        ]


getMenuAtPath path =
    getMenuItemAtPath path menu |> Result.map getMenu


getName : MenuItem -> String
getName (Node name _) =
    name


getMenu : MenuItem -> Menu
getMenu (Node _ menu) =
    menu


type alias Path =
    List String


getMenuItemAtPath : Path -> MenuItem -> Result String MenuItem
getMenuItemAtPath pss m =
    case ( pss, m ) of
        ( [ "root" ], _ ) ->
            Ok m

        ( [], _ ) ->
            Ok m

        ( p :: ps, Node _ lst ) ->
            let
                go : MenuItem -> Result String MenuItem -> Result String MenuItem
                go (Node m lst) acc =
                    case acc of
                        -- if we've already found it then skip through remainder
                        Ok _ ->
                            acc

                        Err _ ->
                            if m == p then
                                Ok (Node m lst)
                            else
                                acc
            in
                case L.foldl go (Err <| "Could not find " ++ p) lst of
                    Err err ->
                        Err err

                    Ok nextLevel ->
                        getMenuItemAtPath ps nextLevel



--


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
