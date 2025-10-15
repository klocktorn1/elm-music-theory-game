module Game.ModeExercise exposing (..)

import Array
import Browser
import Game.NoteExercise exposing (Msg(..))
import Game.TheoryApi as TheoryApi
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Process
import Random
import Task



{-

    Mode guesser game:

      User chooses key
   -> random mode is applied to that key
   -> major scale with mode applied to is rendered on screen
   -> all seven modes are rendered on screen as buttons
   -> user clicks which mode they think is correct
   -> check if user is correct or not


   Mode builder game:

      User chooses key (example C)
   -> Randomized mode is assigned (Example dorian)
   -> All notes that exist are rendered (A to G including flats and sharps)
   -> User gets prompted to build the dorian scale in C
   -> User has to press the notes in the correct order.
   -> When dorian mode is assembled correctly, score incremented

-}


type alias Model =
    { modes : Maybe TheoryApi.Modes
    , chosenGameMode : Maybe GameMode
    , majorScales : Maybe (List TheoryApi.MajorScale)
    , chosenKey : Maybe TheoryApi.MajorScale
    , randomizedMode : TheoryApi.Mode
    , modesErrorMessage : Maybe String
    , errorMessage : Maybe String
    , modeGuessed : Maybe String
    , isCorrect : Bool
    , isWrong : Bool
    , score : Int
    , resultMessage : String
    , numberOfWrongs : Int
    , allNotes : Maybe (List String)
    , userBuiltMode : List String
    }


type Msg
    = TheoryDbFetched (Result Http.Error TheoryApi.TheoryDb)
    | ChooseGame GameMode
    | ChooseKey TheoryApi.MajorScale
    | PickRandomMode Int
    | ModeGuessed String
    | RandomizeMode
    | ResetWrong
    | GoBack
    | AddToModeBuilderList String
    | SubmitBuiltMode


type alias Flags =
    String


type GameMode
    = ModeGuesserGame
    | ModeBuilderGame


fetchTheoryDb : Cmd Msg
fetchTheoryDb =
    TheoryApi.fetchTheoryDb TheoryDbFetched


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { modes = Nothing
      , chosenGameMode = Nothing
      , majorScales = Nothing
      , chosenKey = Nothing
      , randomizedMode = { mode = "No mode randomized yet", formula = [] }
      , modesErrorMessage = Nothing
      , errorMessage = Nothing
      , modeGuessed = Nothing
      , isCorrect = False
      , isWrong = False
      , score = 0
      , resultMessage = ""
      , numberOfWrongs = 0
      , allNotes = Nothing
      , userBuiltMode = []
      }
    , Cmd.batch [ fetchTheoryDb ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseGame gameMode ->
            ( { model | chosenGameMode = Just gameMode }, Cmd.none )

        GoBack ->
            ( { model | chosenGameMode = Nothing }, Cmd.none )

        TheoryDbFetched (Ok theoryDb) ->
            ( { model
                | modes = Just theoryDb.modes
                , majorScales = Just theoryDb.majorScales
                , allNotes = Just theoryDb.allNotes
              }
            , Cmd.none
            )

        TheoryDbFetched (Err httpError) ->
            ( { model | errorMessage = Just (TheoryApi.buildErrorMessage httpError) }, Cmd.none )

        ChooseKey key ->
            let
                newModel =
                    { model
                        | chosenKey = Just key
                        , resultMessage = ""
                    }
            in
            ( newModel, randomizeMode )

        PickRandomMode randomIndex ->
            ( { model | randomizedMode = pickRandomMode model.modes randomIndex, isCorrect = False }, Cmd.none )

        ModeGuessed mode ->
            let
                modelWithModeGuessed =
                    { model | modeGuessed = Just mode }

                modelWithResult =
                    checkIfModeGuessCorrect modelWithModeGuessed
            in
            modelWithResult

        RandomizeMode ->
            ( model, randomizeMode )

        ResetWrong ->
            ( { model | isWrong = False }, Cmd.none )

        AddToModeBuilderList note ->
            ( { model | userBuiltMode = note :: model.userBuiltMode }, Cmd.none )

        SubmitBuiltMode ->
            checkIfModeBuiltCorrect model


view : Model -> Html Msg
view model =
    Html.div []
        [ case model.chosenGameMode of
            Just ModeGuesserGame ->
                Html.div []
                    [ Html.text """Choose a key below. A randomized mode 
                            (ionian, dorian, phrygian, lydian, mixolydian, aeolian or locrian) 
                            will be applied to the chosen key. Choose which mode is correct."""
                    , viewKeysOrError model
                    , viewModeGuesserGame model
                    , Html.button [ HA.class "key-button", HE.onClick GoBack ] [ Html.text "<--- Go back" ]
                    ]

            Just ModeBuilderGame ->
                Html.div []
                    [ Html.text """Choose a key below. A randomized mode 
                            (ionian, dorian, phrygian, lydian, mixolydian, aeolian or locrian) 
                            will be given. Your goal is to build this mode in the chosen key"""
                    , viewKeysOrError model
                    , viewModeBuilderGame model
                    , Html.button [ HA.class "key-button", HE.onClick GoBack ] [ Html.text "<--- Go back" ]
                    ]

            Nothing ->
                Html.div []
                    [ Html.text "Please choose a game mode"
                    , Html.div []
                        [ Html.button [ HE.onClick (ChooseGame ModeGuesserGame), HA.class "key-button" ] [ Html.text "Guess the mode" ]
                        , Html.br [] []
                        , Html.button [ HA.class "key-button", HE.onClick (ChooseGame ModeBuilderGame) ] [ Html.text "Build the mode" ]
                        ]
                    ]
        ]


viewModeBuilderGame : Model -> Html Msg
viewModeBuilderGame model =
    case model.chosenKey of
        Just chosenKey ->
            Html.div []
                [ Html.p [] [ Html.text ("Please build the " ++ chosenKey.key ++ " " ++ model.randomizedMode.mode ++ " mode") ]
                , Html.p []
                    ([]
                        ++ List.reverse (List.map viewUserBuiltMode model.userBuiltMode)
                    )
                , viewAllNotes model
                , Html.button [ HA.class "key-button", HE.onClick SubmitBuiltMode ] [ Html.text "Submit" ]
                ]

        Nothing ->
            Html.div [] [ Html.text "Please choose a key" ]


viewUserBuiltMode : String -> Html Msg
viewUserBuiltMode note =
    Html.text (note ++ " ")


viewAllNotes : Model -> Html Msg
viewAllNotes model =
    case model.allNotes of
        Just allNotes ->
            Html.div []
                (List.map
                    (\note ->
                        Html.button [ HA.class "key-button", HE.onClick (AddToModeBuilderList note) ]
                            [ Html.text (note ++ ",") ]
                    )
                    allNotes
                )

        Nothing ->
            Html.div [] [ Html.text "No notes" ]


viewModeGuesserGame : Model -> Html Msg
viewModeGuesserGame model =
    case model.chosenKey of
        Just chosenKey ->
            Html.div []
                [ viewConstructedMode chosenKey model.randomizedMode.formula
                , viewModes model
                , Html.p [] [ Html.text ("Score: " ++ String.fromInt model.score) ]
                , Html.text model.resultMessage
                , Html.p [] [ Html.text ("Mistakes: " ++ String.fromInt model.numberOfWrongs) ]
                , Html.br [] []
                , Html.br [] []
                , Html.br [] []
                , Html.br [] []
                , Html.p [] [ Html.text ("For testing, correct answer is: " ++ model.randomizedMode.mode) ]
                , Html.p [] [ Html.text (Debug.toString (model.modeGuessed == Just model.randomizedMode.mode)) ]
                , Html.p [] [ Html.text ("modeGuessed:     " ++ Debug.toString model.modeGuessed) ]
                , Html.p [] [ Html.text ("randomizedMode:     " ++ Debug.toString model.randomizedMode.mode) ]
                ]

        Nothing ->
            Html.div [] [ Html.text "Please choose a key" ]


viewModes : Model -> Html Msg
viewModes model =
    case model.modes of
        Just modes ->
            Html.div [] (List.map (viewModeButton model) modes)

        Nothing ->
            Html.div [] []


viewModeButton : Model -> TheoryApi.Mode -> Html Msg
viewModeButton model mode =
    Html.button
        [ HA.classList
            [ ( "correct", isModeCorrect model mode.mode )
            , ( "wrong-" ++ String.fromInt model.numberOfWrongs, isModeWrong model mode.mode )
            ]
        , HE.onClick (ModeGuessed mode.mode)
        , HA.class "key-button"
        , HA.style "margin-right" "20px"
        ]
        [ Html.text mode.mode ]


isModeCorrect : Model -> String -> Bool
isModeCorrect model modeName =
    model.isCorrect && (modeName == model.randomizedMode.mode)


isModeWrong : Model -> String -> Bool
isModeWrong model modeName =
    model.isWrong && (model.modeGuessed == Just modeName)


viewConstructedMode : TheoryApi.MajorScale -> List String -> Html Msg
viewConstructedMode chosenKey mode =
    Html.div [ HA.class "mode-container" ]
        (List.append [ Html.text "Mode: " ]
            (chosenKey
                |> notesToListString
                |> constructMode mode
                |> List.map viewConstructMode
            )
        )


viewKeysOrError : Model -> Html Msg
viewKeysOrError model =
    case model.errorMessage of
        Just error ->
            Html.div []
                [ Html.text error ]

        Nothing ->
            case model.majorScales of
                Just majorScales ->
                    Html.div [ HA.class "key-buttons-container" ] (List.map viewKeyButtons majorScales)

                Nothing ->
                    Html.div [] [ Html.text "something else" ]


viewKeyButtons : TheoryApi.MajorScale -> Html Msg
viewKeyButtons key =
    Html.div []
        [ Html.div [ HA.class "key-button", HE.onClick (ChooseKey key) ]
            [ Html.text key.key
            ]
        ]


viewConstructMode : String -> Html Msg
viewConstructMode constructedNote =
    Html.div []
        [ Html.text constructedNote
        , Html.div [] []
        ]


viewModeButtons : TheoryApi.Mode -> Html Msg
viewModeButtons mode =
    Html.div [ HA.class "key-button" ] [ Html.text mode.mode ]


notesToListString : TheoryApi.MajorScale -> List String
notesToListString key =
    List.map Tuple.second key.notes


constructMode : List String -> List String -> List String
constructMode mode key =
    List.map2 (++) key mode
        |> List.map (String.replace "#b" "")
        |> List.map (String.replace "b#" "")


checkIfModeGuessCorrect : Model -> ( Model, Cmd Msg )
checkIfModeGuessCorrect model =
    if model.modeGuessed == Just model.randomizedMode.mode then
        ( { model
            | score = model.score + 1
            , isCorrect = True
            , resultMessage = "Correct!"
            , isWrong = False
          }
        , Task.perform (\_ -> RandomizeMode) (Process.sleep 500)
        )

    else
        let
            newNumberOfWrongs =
                model.numberOfWrongs + 1

            resetCmd =
                Task.perform (\_ -> ResetWrong) (Process.sleep 500)
        in
        ( { model
            | resultMessage = "Wrong, try again."
            , isWrong = True
            , numberOfWrongs = newNumberOfWrongs
          }
        , resetCmd
        )


checkIfModeBuiltCorrect : Model -> ( Model, Cmd Msg )
checkIfModeBuiltCorrect model =
    case model.chosenKey of
        Just chosenKey ->
            if List.reverse model.userBuiltMode == constructMode model.randomizedMode.formula (notesToListString chosenKey) then
                let
                    _ =
                        Debug.log "You win!" ""
                in
                ( model, Cmd.none )

            else
                ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


pickRandomMode : Maybe TheoryApi.Modes -> Int -> TheoryApi.Mode
pickRandomMode maybeModes randomIndex =
    case maybeModes of
        Just modes ->
            let
                maybeRandomModeObject =
                    case Array.get randomIndex (Array.fromList modes) of
                        Just randomModeObject ->
                            randomModeObject

                        Nothing ->
                            { mode = "No mode found at given index", formula = [] }
            in
            maybeRandomModeObject

        Nothing ->
            { mode = "No mode found", formula = [] }


randomizeMode : Cmd Msg
randomizeMode =
    Random.generate PickRandomMode (Random.int 0 6)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
