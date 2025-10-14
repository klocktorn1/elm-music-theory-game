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


type alias Model =
    { modes : Maybe TheoryApi.Modes
    , chosenGameMode : Maybe GameMode
    , keys : Maybe (List TheoryApi.Key)
    , chosenKey : Maybe TheoryApi.Key
    , randomizedMode : TheoryApi.Mode
    , modesErrorMessage : Maybe String
    , keysErrorMessage : Maybe String
    , modeGuessed : Maybe String
    , isCorrect : Bool
    , isWrong : Bool
    , score : Int
    , resultMessage : String
    , numberOfWrongs : Int
    }


type Msg
    = ModesFetched (Result Http.Error TheoryApi.Modes)
    | ChooseGame GameMode
    | ScalesFetched (Result Http.Error (List TheoryApi.Key))
    | ChooseKey TheoryApi.Key
    | PickRandomMode Int
    | ModeGuessed String
    | RandomizeMode
    | ResetWrong
    | GoBack


type alias Flags =
    String


type GameMode
    = ModeGuesserGame
    | ModeBuilderGame



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


fetchScales : Cmd Msg
fetchScales =
    TheoryApi.fetchScales ScalesFetched


fetchModes : Cmd Msg
fetchModes =
    TheoryApi.fetchModes ModesFetched


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { modes = Nothing
      , chosenGameMode = Nothing
      , keys = Nothing
      , chosenKey = Nothing
      , randomizedMode = { mode = "No mode randomized yet", formula = [] }
      , modesErrorMessage = Nothing
      , keysErrorMessage = Nothing
      , modeGuessed = Nothing
      , isCorrect = False
      , isWrong = False
      , score = 0
      , resultMessage = ""
      , numberOfWrongs = 0
      }
    , Cmd.batch [ fetchScales, fetchModes ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseGame gameMode ->
            ( { model | chosenGameMode = Just gameMode }, Cmd.none )

        GoBack ->
            ( { model | chosenGameMode = Nothing }, Cmd.none )

        ModesFetched (Ok modes) ->
            ( { model | modes = Just modes }, Cmd.none )

        ModesFetched (Err httpError) ->
            ( { model | modesErrorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

        ScalesFetched (Ok keys) ->
            ( { model | keys = Just keys }, Cmd.none )

        ScalesFetched (Err httpError) ->
            ( { model | keysErrorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

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
                    checkIfCorrect modelWithModeGuessed
            in
            modelWithResult

        RandomizeMode ->
            ( model, randomizeMode )

        ResetWrong ->
            ( { model | isWrong = False }, Cmd.none )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later"

        Http.NetworkError ->
            "Unable to reach server"

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ (statusCode |> String.fromInt)

        Http.BadBody message ->
            message


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
                    [ Html.button [ HA.class "key-button", HE.onClick GoBack ] [ Html.text "<--- Go back" ]
                    , viewModeBuilderGame model
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
    Html.div [] []


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


viewConstructedMode : TheoryApi.Key -> List String -> Html Msg
viewConstructedMode chosenKey mode =
    Html.div [ HA.class "mode-container" ]
        (List.append [ Html.text "Mode: " ]
            (chosenKey
                |> notesToListString
                |> constructScale mode
                |> List.map viewConstructedScale
            )
        )


viewKeysOrError : Model -> Html Msg
viewKeysOrError model =
    case model.keysErrorMessage of
        Just error ->
            Html.div []
                [ Html.text error ]

        Nothing ->
            case model.keys of
                Just keys ->
                    Html.div [ HA.class "key-buttons-container" ] (List.map viewKeyButtons keys)

                Nothing ->
                    Html.div [] [ Html.text "something else" ]


viewKeyButtons : TheoryApi.Key -> Html Msg
viewKeyButtons key =
    Html.div []
        [ Html.div [ HA.class "key-button", HE.onClick (ChooseKey key) ]
            [ Html.text key.key
            ]
        ]


viewConstructedScale : String -> Html Msg
viewConstructedScale constructedNote =
    Html.div []
        [ Html.text constructedNote
        , Html.div [] []
        ]


viewModeButtons : TheoryApi.Mode -> Html Msg
viewModeButtons mode =
    Html.div [ HA.class "key-button" ] [ Html.text mode.mode ]


notesToListString : TheoryApi.Key -> List String
notesToListString key =
    List.map Tuple.second key.notes


constructScale : List String -> List String -> List String
constructScale mode key =
    List.map2 (++) key mode
        |> List.map (String.replace "#b" "")
        |> List.map (String.replace "b#" "")


checkIfCorrect : Model -> ( Model, Cmd Msg )
checkIfCorrect model =
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
