port module Game.NoteExercise exposing (..)

import Array exposing (Array)
import Game.TheoryApi as TheoryApi
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Encode as Encode
import Process
import Random
import Random.List as RandomList
import Set exposing (Set)
import Task



{-
   maybe change so that only one random note from the scale is shown instead

   example:

    User chooses key
    -> Scale degrees (1-7) are rendered on screenma
    -> One note from that scale is shown with prompt: Which scale degree is "NOTE" in "KEY" major scale?
    -> User presses the number they think is correct


    isNoteVisible = False
    ->


-}


type alias Model =
    { chosenScale : Maybe TheoryApi.Key
    , scales : List TheoryApi.Key
    , noteClicked : String
    , score : Int
    , correctPairs : Set CorrectPair
    , wrongPairs : Maybe ( Int, Int )
    , noteIndex : Int
    , numberIndex : Int
    , numberOfWins : Int
    , numberOfWrongs : Int
    , isButtonDisabled : Bool
    , gameOver : Bool
    , errorMessage : Maybe String
    , exerciseStep : Int
    }


type alias Flags =
    String


type alias CorrectPair =
    ( Int, Int )


type alias WrongPair =
    ( Int, Int )


type Msg
    = NoteClicked Int
    | NumberClicked Int
    | Shuffle
    | Shuffled (List ( Int, TheoryApi.Note ))
    | ScalesFetched (Result Http.Error (List TheoryApi.Key))
    | ChooseKey TheoryApi.Key
    | Reset Bool
    | GameOver


port sendToLocalStorage : String -> Cmd msg


saveWins : Int -> Cmd msg
saveWins numberOfWins =
    Encode.int numberOfWins
        |> Encode.encode 0
        |> sendToLocalStorage


fetchScales : Cmd Msg
fetchScales =
    TheoryApi.fetchScales ScalesFetched


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { chosenScale = Nothing
      , scales = []
      , noteClicked = ""
      , score = 0
      , correctPairs = Set.empty
      , wrongPairs = Nothing
      , noteIndex = 7
      , numberIndex = 7
      , numberOfWins = String.toInt flags |> Maybe.withDefault 0
      , numberOfWrongs = 0
      , isButtonDisabled = True
      , gameOver = False
      , errorMessage = Nothing
      , exerciseStep = 0
      }
    , Cmd.batch [ fetchScales ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoteClicked index ->
            let
                modelWithWrongPairsSetEmpty =
                    { model | wrongPairs = Nothing }

                modelWithNote =
                    { modelWithWrongPairsSetEmpty | noteIndex = index }
            in
            ( { modelWithNote | isButtonDisabled = False }, Cmd.none )

        NumberClicked number ->
            let
                modelWithNumber =
                    { model | numberIndex = number, wrongPairs = Nothing }

                newModel =
                    checkClickedValues modelWithNumber
            in
            newModel

        Shuffle ->
            case model.chosenScale of
                Just scale ->
                    ( model, Random.generate Shuffled (RandomList.shuffle scale.notes) )

                Nothing ->
                    ( model, Cmd.none )

        Shuffled newNotes ->
            case model.chosenScale of
                Just scale ->
                    let
                        newScale =
                            { scale | notes = newNotes }
                    in
                    ( { model | chosenScale = Just newScale }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ScalesFetched (Ok scales) ->
            ( { model | scales = scales }, Cmd.none )

        ScalesFetched (Err error) ->
            ( model, Cmd.none )

        ChooseKey scale ->
            let
                newModel =
                    ( { model
                        | chosenScale = Just scale
                        , noteClicked = ""
                        , noteIndex = 7
                        , numberIndex = 7
                        , correctPairs = Set.empty
                      }
                    , Random.generate Shuffled (RandomList.shuffle scale.notes)
                    )
            in
            newModel

        Reset shuffle ->
            case model.chosenScale of
                Just scale ->
                    ( { model
                        | chosenScale = Just scale
                        , noteClicked = ""
                        , noteIndex = 7
                        , numberIndex = 7
                        , correctPairs = Set.empty
                        , wrongPairs = Nothing
                        , numberOfWrongs = 0
                        , gameOver = False
                      }
                    , if shuffle then
                        Random.generate Shuffled (RandomList.shuffle scale.notes)

                      else
                        Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        GameOver ->
            ( model, gameOver )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ HA.classList [ ( "game-over-disabled", model.gameOver ) ] ] []
        , Html.div [] [ Html.text "Pick a major scale:" ]
        , Html.text "Pick a key. Match the note with the corresponding number in that scale (C1 D2 E3 F4 G5 A6 B7 in C for example)"
        , Html.div [ HA.class "key-buttons-container" ] (List.map (\key -> viewKeys key) model.scales)
        , case model.chosenScale of
            Just scale ->
                Html.div []
                    [ if model.isButtonDisabled then
                        Html.text "Please start by pressing a note"

                      else
                        Html.text ""
                    , Html.div [ HA.class "note-buttons-container" ]
                        (List.map (\note -> viewScaleButtons model note) scale.notes)
                    , Html.div [ HA.class "number-buttons-container" ]
                        (List.indexedMap (\index _ -> viewNumberButtons model index) [ 1, 2, 3, 4, 5, 6, 7 ])
                    , Html.div [] [ Html.text ("Score: " ++ String.fromInt (Set.size model.correctPairs)) ]
                    , Html.div [] [ Html.text ("Mistakes: " ++ String.fromInt model.numberOfWrongs) ]
                    , Html.br [] []
                    , Html.div [] [ Html.text ("Number of wins: " ++ String.fromInt model.numberOfWins) ]
                    , Html.div []
                        [ Html.button [ HE.onClick (Reset True), HA.class "reset-button" ] [ Html.text "Reset" ] ]
                    , viewGameOverMessage model
                    , Html.text (Debug.toString model.wrongPairs)
                    , Html.text (Debug.toString model.chosenScale)
                    ]

            Nothing ->
                Html.div [] []
        ]


viewGameOverMessage : Model -> Html Msg
viewGameOverMessage model =
    Html.div
        [ HA.class "game-over-message-container"
        , if not model.gameOver then
            HA.style "display" "none"

          else
            HA.style "display" ""
        ]
        [ Html.p [] [ Html.text "Game over" ]
        , Html.p []
            [ Html.button [ HE.onClick GameOver, HA.class "try-again-button" ] [ Html.text "Try again" ] ]
        ]


viewKeys : TheoryApi.Key -> Html Msg
viewKeys scale =
    Html.div [ HA.class "key-button", HE.onClick (ChooseKey scale) ] [ Html.text scale.key ]


viewNumberButtons : Model -> Int -> Html Msg
viewNumberButtons model number =
    Html.button
        [ HE.onClick (NumberClicked number)
        , HA.class "number-button"
        , HA.disabled (model.isButtonDisabled || model.gameOver)
        ]
        [ Html.h1
            [ HA.classList
                [ ( "correct-animation", isNumberCorrect model number )
                , ( "wrong-" ++ String.fromInt model.numberOfWrongs, isNumberWrong model number ) -- Trying to get it to flash red when pressing the same wrong again
                ]
            ]
            [ Html.text (String.fromInt (number + 1)) ]
        ]


viewScaleButtons : Model -> ( Int, String ) -> Html Msg
viewScaleButtons model ( originalIndex, note ) =
    let
        isActive =
            model.noteIndex == originalIndex
    in
    Html.button
        [ HE.onClick (NoteClicked originalIndex)
        , HA.disabled model.gameOver
        ]
        [ Html.h1
            [ HA.class "note-button"
            , HA.classList
                [ ( "correct-animation", isNoteCorrect model originalIndex )
                , ( "game-button-active", isActive )
                , ( "not-visible", isNoteVisible model note )
                ]
            ]
            [ Html.text note
            ]
        ]


isNoteVisible : Model -> String -> Bool
isNoteVisible model note =
    let
        arrayOfNotesChosen =
            case model.chosenScale of
                Just chosenScale ->
                    Array.fromList chosenScale.notes

                Nothing ->
                    Array.empty

        notePrompted =
            case Array.get model.exerciseStep arrayOfNotesChosen of
                Just noteAndIndexTuple ->
                    Tuple.second noteAndIndexTuple

                Nothing ->
                    "No note found at this index"

        _ =
            Debug.log "notePrompted" notePrompted
    in
    notePrompted /= note


isNoteCorrect : Model -> Int -> Bool
isNoteCorrect model noteIndex =
    Set.member noteIndex (Set.map Tuple.first model.correctPairs)


isNumberCorrect : Model -> Int -> Bool
isNumberCorrect model numberIndex =
    Set.member numberIndex (Set.map Tuple.second model.correctPairs)


isNumberWrong : Model -> Int -> Bool
isNumberWrong model number =
    case model.wrongPairs of
        Just wrongPairs ->
            Tuple.second wrongPairs == number

        Nothing ->
            False


checkClickedValues : Model -> ( Model, Cmd Msg )
checkClickedValues model =
    if model.noteIndex == model.numberIndex then
        let
            newPair =
                ( model.noteIndex, model.numberIndex )

            newCorrectPairs =
                Set.insert newPair model.correctPairs

            newScore =
                Set.size newCorrectPairs

            newNumberOfWins =
                model.numberOfWins + 1

            newExerciseStep =
                model.exerciseStep + 1

            newModel =
                if newScore == 7 then
                    ( { model | numberOfWins = newNumberOfWins, exerciseStep = 0 }
                    , Cmd.batch
                        [ saveWins newNumberOfWins
                        , Task.perform (\_ -> Reset True) (Process.sleep 0)
                        ]
                    )

                else
                    ( { model
                        | score = newScore
                        , correctPairs = Set.insert newPair model.correctPairs
                        , exerciseStep = newExerciseStep
                      }
                    , Cmd.none
                    )
        in
        newModel

    else if model.wrongPairs == Nothing then
        let
            newWrongPair =
                ( model.noteIndex, model.numberIndex )

            newNumberOfWrongs =
                model.numberOfWrongs + 1

            newModel =
                if Tuple.second newWrongPair == 7 then
                    model

                else
                    { model
                        | wrongPairs = Just newWrongPair
                        , numberOfWrongs = newNumberOfWrongs
                        , gameOver = newNumberOfWrongs == 100
                    }
        in
        ( newModel, Cmd.none )

    else
        ( model, Cmd.none )


gameOver : Cmd Msg
gameOver =
    Task.perform (\_ -> Reset False) (Process.sleep 0)
