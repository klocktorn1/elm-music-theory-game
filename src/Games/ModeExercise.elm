module Games.ModeExercise exposing (..)

import Array
import Games.NoteExercise exposing (Msg(..))
import Games.TheoryApi as TheoryApi
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Process
import Random
import Task
import Games.NoteBuilder as NoteBuilder



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
    { modes : Maybe (List TheoryApi.Mode)
    , chosenGameMode : Maybe GameMode
    , majorScalesAndKeys : Maybe (List TheoryApi.MajorScaleAndKey)
    , chosenKey : Maybe TheoryApi.MajorScaleAndKey
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
    , result : Maybe SubmitResult
    , gameOver : Bool
    }


type Msg
    = GotTheoryDb TheoryApi.TheoryDb
    | ChooseGame GameMode
    | ChooseKey TheoryApi.MajorScaleAndKey
    | PickRandomMode Int
    | ModeGuessed String
    | RandomizeMode
    | ResetWrong
    | GoBack
    | AddToModeBuilderList String
    | SubmitBuiltMode
    | Submitted SubmitResult
    | Undo
    | Reset


type alias Flags =
    String


type SubmitResult
    = Win
    | Lose


type GameMode
    = ModeGuesserGame
    | ModeBuilderGame


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { modes = Nothing
      , chosenGameMode = Nothing
      , majorScalesAndKeys = Nothing
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
      , result = Nothing
      , gameOver = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTheoryDb db ->
            ( { model
                | modes = Just db.modes
                , majorScalesAndKeys = Just db.majorScalesAndKeys
                , allNotes = Just db.allNotes
              }
            , Cmd.none
            )

        ChooseGame gameMode ->
            ( { model | chosenGameMode = Just gameMode }, Cmd.none )

        GoBack ->
            ( { model | chosenGameMode = Nothing }, Cmd.none )

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

        Submitted result ->
            case result of
                Win ->
                    ( { model | result = Just result }, Cmd.none )

                Lose ->
                    ( { model | result = Just result }, Cmd.none )

        Undo ->
            ( { model | userBuiltMode = List.drop 1 model.userBuiltMode }, Cmd.none )

        Reset ->
            ( { model
                | gameOver = False
                , userBuiltMode = []
                , result = Nothing
              }
            , randomizeMode
            )


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
                    , Html.button [ HA.class "custom-button", HE.onClick GoBack ] [ Html.text "<--- Go back" ]
                    ]

            Just ModeBuilderGame ->
                Html.div []
                    [ Html.text """Choose a key below. A randomized mode 
                            (ionian, dorian, phrygian, lydian, mixolydian, aeolian or locrian) 
                            will be given. Your goal is to build this mode in the chosen key"""
                    , viewKeysOrError model
                    , viewModeBuilderGame model
                    , Html.button [ HA.class "custom-button", HE.onClick GoBack ] [ Html.text "<--- Go back" ]
                    , viewGameOverOrWinMessage model
                    ]

            Nothing ->
                Html.div []
                    [ Html.text "Please choose a game mode"
                    , Html.div []
                        [ Html.button [ HE.onClick (ChooseGame ModeGuesserGame), HA.class "custom-button" ] [ Html.text "Guess the mode" ]
                        , Html.br [] []
                        , Html.button [ HA.class "custom-button", HE.onClick (ChooseGame ModeBuilderGame) ] [ Html.text "Build the mode" ]
                        ]
                    ]
        ]


viewGameOverOrWinMessage : Model -> Html Msg
viewGameOverOrWinMessage model =
    case model.result of
        Just Win ->
            Html.div
                [ HA.class "game-over-message-container"
                , if not model.gameOver then
                    HA.style "display" "none"

                  else
                    HA.style "display" ""
                ]
                [ Html.p [] [ Html.text "You win!" ]
                , Html.p []
                    [ Html.button [ HE.onClick Reset, HA.class "custom-button" ] [ Html.text "Play again" ] ]
                ]

        Just Lose ->
            Html.div
                [ HA.class "game-over-message-container"
                , if not model.gameOver then
                    HA.style "display" "none"

                  else
                    HA.style "display" ""
                ]
                [ Html.p [] [ Html.text "Game over" ]
                , Html.p []
                    [ Html.button [ HE.onClick Reset, HA.class "custom-button" ] [ Html.text "Try again" ] ]
                ]

        Nothing ->
            Html.div [] []


viewModeBuilderGame : Model -> Html Msg
viewModeBuilderGame model =
    case model.chosenKey of
        Just chosenKey ->
            Html.div []
                [ Html.p [] [ Html.text ("Please build the " ++ chosenKey.key ++ " " ++ model.randomizedMode.mode ++ " mode") ]
                , NoteBuilder.viewNotes AddToModeBuilderList model.allNotes chosenKey.key
                , Html.p [ HA.class "user-built-notes-container" ]
                    (List.reverse (List.map viewUserBuiltMode model.userBuiltMode))
                , if model.userBuiltMode == [] then
                    Html.div [] []

                  else
                    Html.div [ HA.class "undo-button-container" ] [ Html.button [ HA.class "custom-button", HE.onClick Undo ] [ Html.text "Undo" ] ]
                , Html.button [ HA.class "custom-button", HE.onClick SubmitBuiltMode ] [ Html.text "Submit" ]
                ]

        Nothing ->
            Html.div [] [ Html.text "Please choose a key" ]


viewUserBuiltMode : String -> Html Msg
viewUserBuiltMode note =
    Html.div [] [ Html.text (note ++ " ") ]





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
        , HA.class "custom-button"
        , HA.style "margin-right" "20px"
        ]
        [ Html.text mode.mode ]


isModeCorrect : Model -> String -> Bool
isModeCorrect model modeName =
    model.isCorrect && (modeName == model.randomizedMode.mode)


isModeWrong : Model -> String -> Bool
isModeWrong model modeName =
    model.isWrong && (model.modeGuessed == Just modeName)


viewConstructedMode : TheoryApi.MajorScaleAndKey -> List String -> Html Msg
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
            case model.majorScalesAndKeys of
                Just majorScalesAndKeys ->
                    Html.div [ HA.class "key-buttons-container" ] (List.map viewKeyButtons majorScalesAndKeys)

                Nothing ->
                    Html.div [] [ Html.text "something else" ]


viewKeyButtons : TheoryApi.MajorScaleAndKey -> Html Msg
viewKeyButtons key =
    Html.div []
        [ Html.div [ HA.class "custom-button", HE.onClick (ChooseKey key) ]
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
    Html.div [ HA.class "custom-button" ] [ Html.text mode.mode ]


notesToListString : TheoryApi.MajorScaleAndKey -> List String
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


isCorrectHelper : Model -> Bool
isCorrectHelper model =
    case model.chosenKey of
        Just chosenKey ->
            List.reverse model.userBuiltMode == constructMode model.randomizedMode.formula (notesToListString chosenKey)

        Nothing ->
            False


checkIfModeBuiltCorrect : Model -> ( Model, Cmd Msg )
checkIfModeBuiltCorrect model =
    if isCorrectHelper model then
        ( { model | gameOver = True }, Task.perform (\_ -> Submitted Win) (Process.sleep 100) )

    else
        ( { model | gameOver = True }, Task.perform (\_ -> Submitted Lose) (Process.sleep 100) )


pickRandomMode : Maybe (List TheoryApi.Mode) -> Int -> TheoryApi.Mode
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


