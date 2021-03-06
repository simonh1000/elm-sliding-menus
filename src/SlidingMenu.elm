module SlidingMenu exposing
    ( view, ViewConfig
    , update, UpdateConfig, subscriptions, Msg
    , Model, init, MenuItem, leaf, node
    )

{-| An Elm library to create animated, nested menus (e.g. for mobile-first websites).


# View

@docs view, ViewConfig


## Update

@docs update, UpdateConfig, subscriptions, Msg


# Setting up

@docs Model, init, MenuItem, leaf, node

-}

import Animation exposing (..)
import Animation.Messenger
import Animation.Spring.Presets exposing (zippy)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Icons exposing (..)
import List as L


{-| Opaque type that you will need to add to your model.

    type alias Model =
        { menu : SlidingMenu.Model
        , ...
        }

-}
type Model
    = Model State


type alias State =
    { pages : MenuChoices
    , previous : Animation.Messenger.State Msg
    , current : Animation.Messenger.State Msg
    , next : Animation.Messenger.State Msg
    }


{-| Opaque type: MenuChoices (previous choices in leaf --> root) current (maybe next one)
-}
type MenuChoices
    = MenuChoices (List String) String (Maybe String)


{-| Opaque type: models a heirarchical menu.
-}
type MenuItem
    = Node String (List MenuItem)


{-| Helper function to build parent MenuItem
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
    subscriptions { menu } =
        Sub.map MenuMsg (SlidingMenu.subscriptions menu)

-}
subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Animation.subscription Animate [ model.previous, model.current, model.next ]


{-| init
-}
init : Model
init =
    Model <| reset <| MenuChoices [] "root" Nothing


reset : MenuChoices -> State
reset pages =
    let
        mkLeft x =
            Animation.style [ Animation.left (percent x) ]
    in
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
                        SlidingMenu.update myUpdateConfig msg model.menu

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
update config message (Model model) =
    let
        ( state, cmd, mbMsg ) =
            update_ config message model
    in
    ( Model state, cmd, mbMsg )


update_ : UpdateConfig -> Msg -> State -> ( State, Cmd Msg, Maybe (List String) )
update_ config message model =
    case message of
        MoveUp ->
            let
                previous =
                    queueWithMsg config OnUpComplete 0.0 model.previous

                current =
                    animateCurrent config 100.0 model.current
            in
            ( { model | previous = previous, current = current }, Cmd.none, Nothing )

        MoveDown chosen ->
            let
                current =
                    animateCurrent config -100.0 model.current

                next =
                    queueWithMsg config OnDownComplete 0.0 model.next

                pages =
                    case model.pages of
                        MenuChoices ps curr _ ->
                            MenuChoices ps curr (Just chosen)
            in
            ( { model | pages = pages, current = current, next = next }, Cmd.none, Nothing )

        OnDownComplete ->
            let
                pages =
                    case model.pages of
                        MenuChoices ps curr (Just nxt) ->
                            MenuChoices (curr :: ps) nxt Nothing

                        _ ->
                            model.pages
            in
            ( reset pages, Cmd.none, Just <| toList pages )

        OnUpComplete ->
            let
                pages =
                    case model.pages of
                        MenuChoices (p :: ps) _ _ ->
                            MenuChoices ps p Nothing

                        _ ->
                            -- TODO see whether we make this state impossible
                            model.pages
            in
            ( reset pages, Cmd.none, Just <| toList pages )

        OnLeafClick s ->
            let
                pages =
                    case model.pages of
                        MenuChoices ps curr _ ->
                            MenuChoices ps curr (Just s)
            in
            ( model, Cmd.none, Just (toList pages) )

        Animate msg ->
            let
                ( next, cmdNext ) =
                    Animation.Messenger.update msg model.next

                ( previous, cmdPrev ) =
                    Animation.Messenger.update msg model.previous
            in
            ( { model
                | previous = previous
                , current = Animation.update msg model.current
                , next = next
              }
            , Cmd.batch [ cmdNext, cmdPrev ]
            , Nothing
            )


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


{-| Data required for the view function. `menu` is the data for the different layers of the menu; see below for how to make `MenuItem`s.
`back` is the string that will be used alongside a "<" to make the back item.
-}
type alias ViewConfig =
    { menu : List MenuItem
    , back : String
    }


{-| Renders the menu. The CSS heirarchy is:

    .sliding
        - menu
        - container
            ul.sliding
        - menu
        - page.previous
            ul.sliding
        - menu
        - page.current
            ul.sliding
        - menu
        - page.next

Note that `.sliding-menu-page` is `display : relative`, and `.sliding-menu-page` is `display : absolute`,
and the library animates the `left` style. You will need to add an appropriate `height` to `.sliding-menu-page`

-}
view : ViewConfig -> Model -> Html Msg
view config (Model state) =
    view_ config state


view_ : ViewConfig -> State -> Html Msg
view_ config state =
    div (class "sliding-menu-container" :: containerStyles) <|
        case state.pages of
            MenuChoices pps curr mbn ->
                [ case pps of
                    p :: ps ->
                        viewPage config "previous" state.previous (MenuChoices ps p (Just curr))

                    [] ->
                        text ""
                , viewPage config "current" state.current state.pages
                , case mbn of
                    Just next ->
                        viewPage config "next" state.next (MenuChoices (curr :: pps) next Nothing)

                    Nothing ->
                        text ""
                ]


viewPage : ViewConfig -> String -> Animation.Messenger.State Msg -> MenuChoices -> Html Msg
viewPage config lab st (MenuChoices ps curr _) =
    case getMenuAtPath (L.reverse <| curr :: ps) [ node "root" config.menu ] of
        Err err ->
            text err

        Ok lst ->
            ul (class ("sliding-menu-page " ++ lab) :: ulStyles ++ Animation.render st) <|
                (if ps == [] then
                    text ""

                 else
                    li (onClick MoveUp :: liStyles)
                        [ span backStyles
                            [ chevronLeft, text config.back ]
                        ]
                )
                    :: L.map mkLi lst


mkLi : MenuItem -> Html Msg
mkLi (Node item lst) =
    if lst == [] then
        li (onClick (OnLeafClick item) :: liStyles)
            [ text item ]

    else
        li (onClick (MoveDown item) :: liStyles)
            [ span [] [ text item ]
            , chevronRight
            ]


backStyles : List (Attribute msg)
backStyles =
    [ Attr.style "display" "flex"
    , Attr.style "align-items" "center"
    , Attr.style "margin-left" "-7px"
    ]


containerStyles : List (Attribute msg)
containerStyles =
    [ Attr.style "overflow" "hidden"
    , Attr.style "position" "relative"
    ]


ulStyles : List (Attribute msg)
ulStyles =
    [ Attr.style "position" "absolute"
    , Attr.style "list-style" "none"
    , Attr.style "margin" "0"
    , Attr.style "padding" "0"
    , Attr.style "left" "0"
    , Attr.style "width" "100%"
    ]


liStyles : List (Attribute msg)
liStyles =
    [ Attr.style "padding" "3px 8px 3px 15px"
    , Attr.style "display" "flex"
    , Attr.style "align-items" "center"
    , Attr.style "justify-content" "space-between"
    , Attr.style "cursor" "default"
    ]



-- Menu Choices (private)


toList : MenuChoices -> List String
toList (MenuChoices ps curr nxt) =
    nxt
        |> Maybe.map List.singleton
        |> Maybe.withDefault []
        |> (\a -> (++) a (curr :: ps))
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
