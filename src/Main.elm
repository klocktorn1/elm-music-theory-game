module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Games.ChordExercise as ChordExercise
import Games.ModeExercise as ModeExercise
import Games.NoteExercise as NoteExercise
import Games.FretboardGame as FretboardGame
import Games.TheoryApi as TheoryApi
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import RemoteData
import Url
import Url.Parser as UP exposing ((</>), (<?>))


type alias Model =
    { url : Url.Url
    , key : Nav.Key
    , route : Route
    , theoryDb : RemoteData.WebData TheoryApi.TheoryDb
    , noteExerciseModel : NoteExercise.Model
    , modeExerciseModel : ModeExercise.Model
    , chordExerciseModel : ChordExercise.Model
    , fretboardGameModel : FretboardGame.Model
    }


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | NoteExerciseMsg NoteExercise.Msg
    | ModeExerciseMsg ModeExercise.Msg
    | ChordExerciseMsg ChordExercise.Msg
    | FretboardGameMsg FretboardGame.Msg
    | GotTheoryDb (Result Http.Error TheoryApi.TheoryDb)


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
            
        ( fretboardGameModel, fretboardGameCmd ) =
            FretboardGame.init ()
    in
    ( { url = url
      , key = key
      , route = route
      , noteExerciseModel = noteExerciseModel
      , modeExerciseModel = modeExerciseModel
      , chordExerciseModel = chordExerciseModel
      , fretboardGameModel = fretboardGameModel
      , theoryDb = RemoteData.Loading
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
        , Cmd.map
            FretboardGameMsg
            fretboardGameCmd
        , TheoryApi.fetchTheoryDb GotTheoryDb
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

        FretboardGameMsg subMsg ->
            let
                ( updatedGameModel, cmd ) =
                    FretboardGame.update subMsg model.fretboardGameModel 
            in
            ( { model | fretboardGameModel = updatedGameModel }
            , Cmd.map FretboardGameMsg cmd
            )

        GotTheoryDb (Ok db) ->
            let
                ( updatedNoteModel, noteCmd ) =
                    NoteExercise.update (NoteExercise.GotTheoryDb db) model.noteExerciseModel

                ( updatedModeModel, modeCmd ) =
                    ModeExercise.update (ModeExercise.GotTheoryDb db) model.modeExerciseModel

                ( updatedChordModel, chordCmd ) =
                    ChordExercise.update (ChordExercise.GotTheoryDb db) model.chordExerciseModel 
            in
            ( { model
                | theoryDb = RemoteData.Success db
                , noteExerciseModel = updatedNoteModel
                , modeExerciseModel = updatedModeModel
                , chordExerciseModel = updatedChordModel
              }
            , Cmd.batch 
                [ Cmd.map NoteExerciseMsg noteCmd
                , Cmd.map ModeExerciseMsg modeCmd
                , Cmd.map ChordExerciseMsg chordCmd
                ]
            )

        GotTheoryDb (Err error) ->
            ( model, Cmd.none )


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
                , Html.li [] [ viewLink "Fretboard Game" "/game/fretboard-game" currentPath ]
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

                "fretboard-game" ->
                    Html.map FretboardGameMsg (FretboardGame.view model.fretboardGameModel)

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
