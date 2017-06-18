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

{-| An Elm library to create animated, nested menus for mobile-first websites.


# View

@docs view, ViewConfig


## Update

@docs update, UpdateConfig, subscriptions, Msg


# Setting up

@docs Model, init, MenuItem, leaf, node

-}

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import List as L
import Animation exposing (..)
import Animation.Messenger
import Animation.Spring.Presets exposing (zippy)
import Icons exposing (..)


{-| Opaque type that you will need to add to your model.

    type alias Model =
        { menu : SlidingMenu.Model
        , ...
        }

-}
type Model
    = Model
        { pages : MenuChoices
        , previous : Animation.Messenger.State Msg
        , current : Animation.Messenger.State Msg
        , next : Animation.Messenger.State Msg
        }


{-| Private type:
MenuChoices (previous choices in leaf --> root) current (maybe bext one)
-}
type MenuChoices
    = MenuChoices (List String) String (Maybe String)


{-| Opaque type that models a heirarchical menu.
-}
type MenuItem
    = Node String (List MenuItem)


{-| Helper function build parent MenuItem
-}
node : String -> List MenuItem -> MenuItem
node =
    Node


{-| Helper function build a leaf MenuItem
-}
leaf : String -> MenuItem
leaf s =
    Node s []


{-| This subscription must be added in your `main` function.

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


{-| Data required for the update function. At present, menu is not used but is required here to prevent
a breaking change down the line. The `easing` function can be any elm-animation [Interpolation](http://package.elm-lang.org/packages/mdgriffith/elm-style-animation/3.5.5/Animation#Interpolation) with the default
set to ['zippy' spring](http://package.elm-lang.org/packages/mdgriffith/elm-style-animation/3.5.5/Animation-Spring-Presets).
-}
type alias UpdateConfig =
    { menu : List MenuItem
    , easing : Maybe Animation.Interpolation
    }


{-| update returns a 3-tuple of the model, a `Cmd SlidingMenu.Msg`, and - following a user click - the menu path chosen by the user.

    update message model =
        case message of
            MenuMsg msg ->
                let
                    ( menu, cmd, maybePath ) =
                        menu.update myUpdateConfig msg model.menu

                    newModel =
                        { model
                            | menu = menu
                            , chosenPath =
                                maybePath
                                    |> Maybe.withDefault model.chosenPath
                        }
                in
                    ( newModel, Cmd.map MenuMsg cmd )

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

        Animate animenusg ->
            let
                ( next, cmdNext ) =
                    Animation.Messenger.update animenusg m.next

                ( previous, cmdPrev ) =
                    Animation.Messenger.update animenusg m.previous
            in
                ( Model
                    { m
                        | previous = previous
                        , current = Animation.update animenusg m.current
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


{-| Data required for the view function. Menu is the data for the different layers of the menu. Back is
the string that will be used alongside a "<" to make the back item.
-}
type alias ViewConfig =
    { menu : List MenuItem
    , back : String
    }


{-| Renders the menu. The CSS heirarchy is:

    .sliding-menu-container
        ul.sliding-menu-page.previous
        ul.sliding-menu-page.current
        ul.sliding-menu-page.next

Note that `.sliding-menu-page` is `display : relative`, and `.sliding-menu-page` is `display : absolute`.
You will need to add an appropriate `height` to `.sliding-menu-page`

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
    case getMenuAtPath (L.reverse <| curr :: ps) [ node "root" config.menu ] of
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



-- Menu Choices (private)


toList : MenuChoices -> List String
toList (MenuChoices ps curr nxt) =
    nxt
        |> Maybe.map List.singleton
        |> Maybe.withDefault []
        |> flip (++) (curr :: ps)
        |> L.reverse
        |> L.drop 1



-- MenuItem (private)


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
