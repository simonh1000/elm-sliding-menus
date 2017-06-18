module SlidingMenu
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

{-| ....


# Data Structures

@docs Model, UpdateConfig, ViewConfig, MenuItem, Msg


# Standard functions

@docs init, update, view, subscriptions


# Helpers

@docs leaf, node

-}

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import List as L
import Animation exposing (..)
import Animation.Messenger
import Animation.Spring.Presets exposing (zippy)
import Icons exposing (..)


{-| Opaque type
-}
type Model
    = Model
        { pages : MenuChoices
        , previous : Animation.Messenger.State Msg
        , current : Animation.Messenger.State Msg
        , next : Animation.Messenger.State Msg
        }


{-| Data required for the update function. At present, menu is not used but is required here to prevent
a breaking change down the line. In the absence of an easing function, the default 'zippy' spring from
elm-animations is used.
-}
type alias UpdateConfig =
    { menu : MenuItem
    , easing : Maybe Animation.Interpolation
    }


{-| Data required for the view function. Menu is the data for the different layers of the menu. Back is
the string that will be used alongside a "<" to make the back item.
-}
type alias ViewConfig =
    { menu : MenuItem
    , back : String
    }


{-| Helper function build an interim menu item
-}
node : String -> List MenuItem -> MenuItem
node =
    Node


{-| Helper function build a leaf menu item
-}
leaf : String -> MenuItem
leaf s =
    Node s []


{-| This subscription must be added in your main function.

    subscriptions : Model -> Sub Msg
    subscriptions { slidingMenuModel } =
        Sub.map MenuMsg (SlidingMenu.subscriptions slidingMenuModel)

-}
subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Animation.subscription Animate [ model.previous, model.current, model.next ]


{-| -}
init : Model
init =
    reset <| MenuChoices [] "root" Nothing


reset : MenuChoices -> Model
reset pages =
    let
        mkLeft x =
            Animation.style [ Animation.left (percent x) ]
    in
        Model
            { pages = pages
            , previous = mkLeft -100.0
            , current = mkLeft 0.0
            , next = mkLeft 100.0
            }



-- UPDATE


{-| Opaque type
-}
type Msg
    = MoveUp
    | MoveDown String -- takes name of next choice
    | OnUpComplete -- Fired after transition up menu
    | OnDownComplete
    | OnLeafClick String
    | Animate Animation.Msg


{-| update returns a 3-tuple of the model, a command and - following a user click - the menu path chosen by the user.

It may be that you only want the leaf value, which is readily available from the path.

    update : Msg -> Model -> ( Model, Cmd Msg )
    update message model =
        case message of
            MenuMsg msg ->
                let
                    ( mm, c1, maybeList ) =
                        MM.update myUpdateConfig msg model.mm

                    newModel =
                        { model | mm = mm, userMessage = maybeList |> Maybe.withDefault model.userMessage }
                in
                    ( newModel, Cmd.map MenuMsg c1 )

-}
update : UpdateConfig -> Msg -> Model -> ( Model, Cmd Msg, Maybe (List String) )
update config message ((Model m) as model) =
    case message of
        OnLeafClick s ->
            let
                pages =
                    case m.pages of
                        MenuChoices ps curr _ ->
                            MenuChoices ps curr (Just s)
            in
                ( model, Cmd.none, Just (toList pages) )

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
                , Nothing
                )

        MoveUp ->
            let
                previous =
                    queueWithMsg config OnUpComplete 0.0 m.previous

                current =
                    animateCurrent config 100.0 m.current
            in
                ( Model { m | previous = previous, current = current }, Cmd.none, Nothing )

        MoveDown chosen ->
            let
                current =
                    animateCurrent config -100.0 m.current

                next =
                    queueWithMsg config OnDownComplete 0.0 m.next

                pages =
                    case m.pages of
                        MenuChoices ps curr _ ->
                            MenuChoices ps curr (Just chosen)
            in
                ( Model { m | pages = pages, current = current, next = next }, Cmd.none, Nothing )

        OnDownComplete ->
            let
                pages =
                    case m.pages of
                        MenuChoices ps curr (Just nxt) ->
                            MenuChoices (curr :: ps) nxt Nothing

                        _ ->
                            m.pages
            in
                ( reset pages, Cmd.none, Just <| toList pages )

        OnUpComplete ->
            let
                pages =
                    case m.pages of
                        MenuChoices (p :: ps) _ _ ->
                            MenuChoices ps p Nothing

                        _ ->
                            Debug.log "Unexpected error MoveUp" m.pages
            in
                ( reset pages, Cmd.none, Just <| toList pages )


animateCurrent : UpdateConfig -> Float -> Animation.Messenger.State Msg -> Animation.Messenger.State Msg
animateCurrent config x stateAnimation =
    Animation.queue [ Animation.toWith (animationWithDefault config) [ Animation.left (percent x) ] ] stateAnimation


queueWithMsg : UpdateConfig -> Msg -> Float -> Animation.Messenger.State Msg -> Animation.Messenger.State Msg
queueWithMsg config msg x state =
    Animation.queue
        [ Animation.toWith (animationWithDefault config) [ Animation.left (percent x) ]
        , Animation.Messenger.send msg
        ]
        state


animationWithDefault : UpdateConfig -> Animation.Interpolation
animationWithDefault { easing } =
    easing |> Maybe.withDefault (Animation.spring zippy)



-- VIEW


{-| Classes

    sliding-menu-container
    sliding-menu-page
        current
        previous
        next

-}
view : ViewConfig -> Model -> Html Msg
view config ((Model m) as model) =
    div [ class "sliding-menu-container", containerStyles ] <|
        case m.pages of
            MenuChoices pps curr mbn ->
                [ case pps of
                    p :: ps ->
                        viewPage config "previous" m.previous (MenuChoices (ps) p (Just curr))

                    [] ->
                        text ""
                , viewPage config "current" m.current m.pages
                , case mbn of
                    Just next ->
                        viewPage config "next" m.next (MenuChoices (curr :: pps) next Nothing)

                    Nothing ->
                        text ""
                ]


viewPage : ViewConfig -> String -> Animation.Messenger.State Msg -> MenuChoices -> Html Msg
viewPage config lab st (MenuChoices ps curr _) =
    case getMenuAtPath (L.reverse <| curr :: ps) [ config.menu ] of
        Err err ->
            text err

        Ok lst ->
            ul (ulStyles :: class ("sliding-menu-page " ++ lab) :: Animation.render st) <|
                (if ps == [] then
                    text ""
                 else
                    li [ onClick MoveUp, liStyles ]
                        [ span [ backStyles ]
                            [ arrow2, text config.back ]
                        ]
                )
                    :: L.map mkLi lst


mkLi : MenuItem -> Html Msg
mkLi (Node item lst) =
    if lst == [] then
        li [ onClick (OnLeafClick item), liStyles ] [ text item ]
    else
        li [ onClick (MoveDown item), liStyles ]
            [ span [] [ text item ]
            , arrow
            ]


backStyles : Attribute msg
backStyles =
    Attr.style
        [ ( "display", "flex" )
        , ( "align-items", "center" )
        , ( "margin-left", "-7px" )
        ]


containerStyles : Attribute msg
containerStyles =
    Attr.style
        [ ( "overflow", "hidden" )
        , ( "position", "relative" )
        ]


ulStyles : Attribute msg
ulStyles =
    Attr.style
        [ ( "position", "absolute" )
        , ( "list-style", "none" )
        , ( "margin", "0" )
        , ( "padding", "0" )
        , ( "left", "0" )
        , ( "width", "100%" )
        ]


liStyles : Attribute msg
liStyles =
    Attr.style
        [ ( "padding", "3px 8px 3px 15px" )
        , ( "display", "flex" )
        , ( "align-items", "center" )
        , ( "justify-content", "space-between" )
        , ( "width", "100%" )
        , ( "cursor", "default" )
        ]



-- Menu Choices


{-| List of previous menu choices in reverse order
-}
type MenuChoices
    = MenuChoices (List String) String (Maybe String)


toList : MenuChoices -> List String
toList (MenuChoices ps curr nxt) =
    nxt
        |> Maybe.map List.singleton
        |> Maybe.withDefault []
        |> flip (++) (curr :: ps)
        |> L.reverse
        |> L.drop 1



-- MenuItem (private)


type MenuItem
    = Node String (List MenuItem)


getMenuAtPath : List String -> List MenuItem -> Result String (List MenuItem)
getMenuAtPath pss m =
    case pss of
        [] ->
            Ok m

        p :: ps ->
            case findByIndex p m of
                Ok (Node _ cs) ->
                    getMenuAtPath ps cs

                Err err ->
                    Err err


findByIndex : String -> List MenuItem -> Result String MenuItem
findByIndex tgt lst =
    let
        go ((Node c cs) as p) acc =
            if c == tgt then
                Ok p
            else
                acc
    in
        L.foldl go (Err <| "Could not find " ++ tgt) lst
