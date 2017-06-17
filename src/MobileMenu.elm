module MobileMenu
    exposing
        ( Model
        , UpdateConfig
        , ViewConfig
        , MenuItem
        , Msg
        , init
        , update
        , view
        , subscriptions
        , leaf
        , node
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List as L
import Animation exposing (..)
import Animation.Messenger
import Icons exposing (..)


type
    MenuChoices
    -- List Int in reverse order
    = MenuChoices (List String) String (Maybe String)


type MenuItem
    = Node String Menu


type alias Menu =
    List MenuItem


type alias Path =
    List String


type alias UpdateConfig msg =
    { menu : MenuItem
    , easing : Animation.Interpolation
    , onLeafClick : List String -> msg
    }


type alias ViewConfig =
    { menu : MenuItem
    }



--


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Animation.subscription Animate [ model.previous, model.current, model.next ]



--


type Model
    = Model
        { pages : MenuChoices
        , previous : Animation.Messenger.State Msg
        , current : Animation.Messenger.State Msg
        , next : Animation.Messenger.State Msg
        }


init : Model
init =
    reset <| MenuChoices [] "root" Nothing


reset : MenuChoices -> Model
reset pages =
    Model
        { pages = pages
        , previous =
            mkLeft -100.0
        , current =
            mkLeft 0.0
        , next =
            mkLeft 100.0
        }



-- UPDATE


type Msg
    = MoveUp
    | MoveDown String
    | SwitchUp
    | SwitchDown
    | OnLeafClick String
    | Animate Animation.Msg


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd Msg, Maybe msg )
update config message ((Model m) as model) =
    case message of
        OnLeafClick s ->
            case m.pages of
                MenuChoices ps curr _ ->
                    ( model
                    , Cmd.none
                    , (s :: curr :: ps)
                        |> List.reverse
                        |> List.drop 1
                        |> config.onLeafClick
                        |> Just
                    )

        _ ->
            let
                ( m, c ) =
                    update_ config message model
            in
                ( m, c, Nothing )


update_ : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd Msg )
update_ config message ((Model m) as model) =
    case message of
        MoveUp ->
            let
                previous =
                    queueWithMsg config SwitchUp 0.0 m.previous

                current =
                    animateCurrent config 100.0 m.current
            in
                ( Model { m | previous = previous, current = current }, Cmd.none )

        MoveDown chosen ->
            let
                current =
                    animateCurrent config -100.0 m.current

                next =
                    queueWithMsg config SwitchDown 0.0 m.next

                pages =
                    case m.pages of
                        MenuChoices ps curr _ ->
                            MenuChoices ps curr (Just chosen)
            in
                ( Model { m | pages = pages, current = current, next = next }, Cmd.none )

        SwitchDown ->
            -- Fired after transition down menu
            let
                pages =
                    case m.pages of
                        MenuChoices ps curr (Just nxt) ->
                            MenuChoices (curr :: ps) nxt Nothing

                        _ ->
                            m.pages
            in
                ( reset pages, Cmd.none )

        SwitchUp ->
            -- Fired after transition up menu
            let
                pages =
                    case m.pages of
                        MenuChoices (p :: ps) _ _ ->
                            MenuChoices ps p Nothing

                        _ ->
                            Debug.log "Unexpected error MoveUp" m.pages
            in
                ( reset pages, Cmd.none )

        Animate animMsg ->
            let
                ( next, cmdNext ) =
                    Animation.Messenger.update animMsg m.next

                ( previous, cmdPrev ) =
                    Animation.Messenger.update animMsg m.previous
            in
                ( Model
                    { m
                        | previous = previous
                        , current = Animation.update animMsg m.current
                        , next = next
                    }
                , Cmd.batch [ cmdNext, cmdPrev ]
                )

        _ ->
            ( model, Cmd.none )


animateCurrent : UpdateConfig msg -> Float -> Animation.Messenger.State Msg -> Animation.Messenger.State Msg
animateCurrent config x stateAnimation =
    Animation.queue [ Animation.toWith config.easing [ Animation.left (percent x) ] ] stateAnimation


queueWithMsg : UpdateConfig msg -> Msg -> Float -> Animation.Messenger.State Msg -> Animation.Messenger.State Msg
queueWithMsg config msg x state =
    Animation.queue
        [ Animation.toWith config.easing [ Animation.left (percent x) ]
        , Animation.Messenger.send msg
        ]
        state



-- VIEW


view : ViewConfig -> Model -> Html Msg
view config model =
    div [ class "menu-container" ] (viewPages config model)


viewPages : ViewConfig -> Model -> List (Html Msg)
viewPages config ((Model m) as model) =
    case m.pages of
        MenuChoices pps curr mbn ->
            [ case pps of
                p :: ps ->
                    mkCurrentPage config "previous" m.previous (MenuChoices (ps) p (Just curr))

                [] ->
                    text ""
            , mkCurrentPage config "current" m.current m.pages
            , case mbn of
                Just next ->
                    mkCurrentPage config "next" m.next (MenuChoices (curr :: pps) next Nothing)

                Nothing ->
                    text ""
            ]


mkCurrentPage : ViewConfig -> String -> Animation.Messenger.State Msg -> MenuChoices -> Html Msg
mkCurrentPage config lab st (MenuChoices ps curr _) =
    case getMenuAtPath (L.reverse <| curr :: ps) [ config.menu ] of
        Err err ->
            text err

        Ok lst ->
            ul (class ("page " ++ lab) :: Animation.render st) <|
                (if ps == [] then
                    text ""
                 else
                    li [ onClick MoveUp ] [ text "<" ]
                )
                    :: L.map mkLi lst


mkLi : MenuItem -> Html Msg
mkLi (Node item lst) =
    if lst == [] then
        li [ onClick (OnLeafClick item) ] [ text item ]
    else
        li [ onClick (MoveDown item), class "flex-h spread" ]
            [ span [] [ text item ]
            , span [] [ arrow ]
            ]



-- Menu Choicses


getCurrent : MenuChoices -> String
getCurrent mc =
    case mc of
        MenuChoices _ curr _ ->
            curr



-- MenuItem


node : String -> List MenuItem -> MenuItem
node =
    Node


leaf : String -> MenuItem
leaf s =
    Node s []


getName : MenuItem -> String
getName (Node name _) =
    name


getMenu : MenuItem -> Menu
getMenu (Node _ menu) =
    menu


getMenuAtPath : Path -> Menu -> Result String Menu
getMenuAtPath pss m =
    case pss of
        [] ->
            Ok m

        p :: ps ->
            case findByIndex p m of
                Ok (Node _ m_) ->
                    getMenuAtPath ps m_

                Err err ->
                    Err err


findByIndex : String -> Menu -> Result String MenuItem
findByIndex tgt lst =
    case lst of
        [] ->
            Err <| "Could not find " ++ tgt

        ((Node c cs) as p) :: ps ->
            if c == tgt then
                Ok p
            else
                findByIndex tgt ps



--


mkLeft : Float -> Animation.Messenger.State Msg
mkLeft x =
    Animation.style [ Animation.left (percent x) ]
