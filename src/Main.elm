module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Game.ChordExercise as ChordExercise
import Game.ModeExercise as ModeExercise
import Game.NoteExercise as NoteExercise
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Url
import Url.Parser as UP exposing ((</>), (<?>))


type alias Model =
    { value : Int
    , url : Url.Url
    , key : Nav.Key
    , route : Route
    , noteExerciseModel : NoteExercise.Model
    , modeExerciseModel : ModeExercise.Model
    , chordExerciseModel : ChordExercise.Model
    }


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | NoteExerciseMsg NoteExercise.Msg
    | ModeExerciseMsg ModeExercise.Msg
    | ChordExerciseMsg ChordExercise.Msg


type Route
    = Home
    | Game String
    | NotFound


type alias Flags =
    String


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            UP.parse routeParser url
                |> Maybe.withDefault NotFound

        ( noteExerciseModel, noteExerciseCmd ) =
            NoteExercise.init flags

        ( modeExerciseModel, modeExerciseCmd ) =
            ModeExercise.init flags

        ( chordExerciseModel, chordExerciseCmd ) =
            ChordExercise.init flags
    in
    ( { value = 0
      , url = url
      , key = key
      , route = route
      , noteExerciseModel = noteExerciseModel
      , modeExerciseModel = modeExerciseModel
      , chordExerciseModel = chordExerciseModel
      }
    , Cmd.batch
        [ Cmd.map
            NoteExerciseMsg
            noteExerciseCmd
        , Cmd.map
            ModeExerciseMsg
            modeExerciseCmd
        , Cmd.map
            ChordExerciseMsg
            chordExerciseCmd
        ]
    )


routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Home (UP.s "home")
        , UP.map Home UP.top
        , UP.map Game (UP.s "game" </> UP.string)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged newUrl ->
            let
                newRoute =
                    UP.parse routeParser newUrl
                        |> Maybe.withDefault NotFound
            in
            ( { model | url = newUrl, route = newRoute }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        NoteExerciseMsg subMsg ->
            let
                ( updatedGameModel, cmd ) =
                    NoteExercise.update subMsg model.noteExerciseModel
            in
            ( { model | noteExerciseModel = updatedGameModel }
            , Cmd.map NoteExerciseMsg cmd
            )

        ModeExerciseMsg subMsg ->
            let
                ( updatedGameModel, cmd ) =
                    ModeExercise.update subMsg model.modeExerciseModel
            in
            ( { model | modeExerciseModel = updatedGameModel }
            , Cmd.map ModeExerciseMsg cmd
            )

        ChordExerciseMsg subMsg ->
            let
                ( updatedGameModel, cmd ) =
                    ChordExercise.update subMsg model.chordExerciseModel
            in
            ( { model | chordExerciseModel = updatedGameModel }
            , Cmd.map ChordExerciseMsg cmd
            )


view : Model -> Browser.Document Msg
view model =
    { title = viewTitle model.route
    , body =
        [ Html.div [ HA.class "wrapper" ]
            [ viewHeader model.url.path
            , Html.main_ [ HA.class "content-container" ]
                [ viewRoute model
                ]
            , viewFooter
            ]
        ]
    }


viewTitle : Route -> String
viewTitle route =
    case route of
        Home ->
            "Home"

        Game gameName ->
            gameName

        NotFound ->
            "Page not found"


viewHeader : String -> Html Msg
viewHeader currentPath =
    Html.div [ HA.class "header-container" ]
        [ Html.header [ HA.class "header" ]
            [ Html.ul []
                [ Html.li [] [ viewLink "Home" "/home" currentPath ]
                , Html.li [] [ viewLink "Note exercise" "/game/note-exercise" currentPath ]
                , Html.li [] [ viewLink "Mode exercise" "/game/mode-exercise" currentPath ]
                , Html.li [] [ viewLink "Chord Exercise" "/game/chord-exercise" currentPath ]
                ]
            ]
        ]


viewRoute : Model -> Html Msg
viewRoute model =
    case model.route of
        Home ->
            Html.div [] [ Html.text "You are at the home page" ]

        Game gameName ->
            case gameName of
                "note-exercise" ->
                    Html.map NoteExerciseMsg (NoteExercise.view model.noteExerciseModel)

                "mode-exercise" ->
                    Html.map ModeExerciseMsg (ModeExercise.view model.modeExerciseModel)

                "chord-exercise" ->
                    Html.map ChordExerciseMsg (ChordExercise.view model.chordExerciseModel)

                _ ->
                    Html.text ("Unknown game: " ++ gameName)

        NotFound ->
            Html.div []
                [ Html.text "Page not found" ]


viewLink : String -> String -> String -> Html Msg
viewLink label path currentPath =
    let
        maybeUrl =
            Url.fromString ("http://localhost:8000" ++ path)

        isActive =
            path == currentPath
    in
    case maybeUrl of
        Just url ->
            Html.a
                [ HA.href path
                , HA.classList [ ( "active", isActive ) ]
                , HE.onClick (LinkClicked (Browser.Internal url))
                ]
                [ Html.text label ]

        Nothing ->
            Html.text ("Invalid Url: " ++ path)


viewFooter : Html Msg
viewFooter =
    Html.div [ HA.class "footer-container" ]
        [ Html.footer [ HA.class "footer" ] []
        ]
