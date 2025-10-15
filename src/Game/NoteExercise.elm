port module Game.NoteExercise exposing (..)

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

   when clicking the same wron two times in a row it doesnt go red.
   when clicking the third mistake and game ends button doesnt go red


-}


type alias Model =
    { chosenScale : Maybe TheoryApi.MajorScale
    , majorScales : Maybe (List TheoryApi.MajorScale)
    , score : Int
    , correctPairs : Set CorrectPair
    , wrongPair : Maybe ( Int, Int )
    , numberIndex : Int
    , numberOfWins : Int
    , numberOfWrongs : Int
    , isButtonDisabled : Bool
    , gameOver : Bool
    , userWins : Bool
    , errorMessage : Maybe String
    , exerciseStep : Int
    , noteAndOriginalIndexTuplePrompted : Maybe ( Int, TheoryApi.Note )
    , result : Maybe GameFinished
    }


type alias Flags =
    String


type alias CorrectPair =
    ( Int, Int )


type alias WrongPair =
    ( Int, Int )


type Msg
    = NumberClicked Int
    | Shuffle
    | Shuffled (List ( Int, TheoryApi.Note ))
    | TheoryDbFetched (Result Http.Error TheoryApi.TheoryDb)
    | ChooseKey TheoryApi.MajorScale
    | Reset Bool


type GameFinished
    = Win
    | Lose


port sendToLocalStorage : String -> Cmd msg


saveWins : Int -> Cmd msg
saveWins numberOfWins =
    Encode.int numberOfWins
        |> Encode.encode 0
        |> sendToLocalStorage


fetchTheoryDb : Cmd Msg
fetchTheoryDb =
    TheoryApi.fetchTheoryDb TheoryDbFetched


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { chosenScale = Nothing
      , majorScales = Nothing
      , score = 0
      , correctPairs = Set.empty
      , wrongPair = Nothing
      , numberIndex = 7
      , numberOfWins = String.toInt flags |> Maybe.withDefault 0
      , numberOfWrongs = 0
      , isButtonDisabled = True
      , gameOver = False
      , userWins = False
      , errorMessage = Nothing
      , exerciseStep = 0
      , noteAndOriginalIndexTuplePrompted = Nothing
      , result = Nothing
      }
    , Cmd.batch [ fetchTheoryDb ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NumberClicked number ->
            let
                modelWithNumber =
                    { model | numberIndex = number, wrongPair = Nothing }

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

                        maybeFirstNote =
                            List.head newNotes
                    in
                    case maybeFirstNote of
                        Just firstNote ->
                            ( { model
                                | chosenScale = Just newScale
                                , noteAndOriginalIndexTuplePrompted = Just firstNote
                                , exerciseStep = 0
                                , correctPairs = Set.empty
                                , wrongPair = Nothing
                                , numberIndex = 7
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        TheoryDbFetched (Ok theoryDb) ->
            ( { model | majorScales = Just theoryDb.majorScales }, Cmd.none )

        TheoryDbFetched (Err httpError) ->
            ( { model | errorMessage = Just (TheoryApi.buildErrorMessage httpError) }, Cmd.none )

        ChooseKey scale ->
            let
                newModel =
                    ( { model
                        | chosenScale = Just scale
                        , score = 0
                        , numberIndex = 7
                        , correctPairs = Set.empty
                        , wrongPair = Nothing
                        , numberOfWrongs = 0
                        , gameOver = False
                        , noteAndOriginalIndexTuplePrompted = Nothing
                      }
                    , Random.generate Shuffled (RandomList.shuffle scale.notes)
                    )
            in
            newModel

        Reset shuffle ->
            case model.chosenScale of
                Just scale ->
                    let
                        firstNote =
                            List.head scale.notes
                    in
                    ( { model
                        | chosenScale = Just scale
                        , numberIndex = 7
                        , correctPairs = Set.empty
                        , wrongPair = Nothing
                        , numberOfWrongs = 0
                        , gameOver = False
                        , noteAndOriginalIndexTuplePrompted = firstNote
                        , exerciseStep = 0
                      }
                    , if shuffle then
                        Random.generate Shuffled (RandomList.shuffle scale.notes)

                      else
                        Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ HA.classList [ ( "game-over-disabled", model.gameOver ) ] ] []
        , Html.div [] [ Html.text "Pick a major scale:" ]
        , Html.text "Pick a key. Match the note with the corresponding number in that scale (C1 D2 E3 F4 G5 A6 B7 in C for example)"
        , viewKeysOrError model
        , case model.chosenScale of
            Just scale ->
                Html.div []
                    [ if model.isButtonDisabled then
                        Html.text "Please start by pressing a note"

                      else
                        Html.text ""
                    , Html.div [ HA.class "note-buttons-container" ] [ viewNotePrompted model ]
                    , Html.div [ HA.class "number-buttons-container" ]
                        (List.indexedMap (\index _ -> viewNumberButtons model index) [ 1, 2, 3, 4, 5, 6, 7 ])
                    , Html.div [] [ Html.text ("Score: " ++ String.fromInt (Set.size model.correctPairs)) ]
                    , Html.div [] [ Html.text ("Mistakes: " ++ String.fromInt model.numberOfWrongs) ]
                    , Html.br [] []
                    , Html.div [] [ Html.text ("Number of wins: " ++ String.fromInt model.numberOfWins) ]
                    , Html.div []
                        [ Html.button [ HE.onClick (Reset True), HA.class "reset-button" ] [ Html.text "Reset" ] ]
                    , viewGameOverOrWinMessage model
                    , Html.p [] [ Html.text ("numberIndex: " ++ Debug.toString model.numberIndex) ]
                    , Html.p [] [ Html.text ("noteAndOriginalIndexTuplePrompted: " ++ Debug.toString model.noteAndOriginalIndexTuplePrompted) ]
                    , Html.p [] [ Html.text ("correctPairs: " ++ Debug.toString model.correctPairs) ]
                    ]

            Nothing ->
                Html.div [] []
        ]


viewNotePrompted : Model -> Html Msg
viewNotePrompted model =
    case model.noteAndOriginalIndexTuplePrompted of
        Just ( _, note ) ->
            Html.div []
                [ Html.h1 []
                    [ Html.text note
                    ]
                ]

        Nothing ->
            Html.div [] [ Html.text "Please choose a key" ]


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
                    [ Html.button [ HE.onClick (Reset True), HA.class "try-again-button" ] [ Html.text "Play again" ] ]
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
                    [ Html.button [ HE.onClick (Reset False), HA.class "try-again-button" ] [ Html.text "Try again" ] ]
                ]

        Nothing ->
            Html.div [] []


viewKeysOrError : Model -> Html Msg
viewKeysOrError model =
    case model.errorMessage of
        Just errorMessage ->
            Html.div [] [ Html.text errorMessage ]

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


viewNumberButtons : Model -> Int -> Html Msg
viewNumberButtons model number =
    Html.button
        [ HE.onClick (NumberClicked number)
        , HA.class "number-button"
        , HA.disabled model.gameOver
        ]
        [ Html.h1
            [ HA.classList
                [ ( "correct-animation", isNumberCorrect model number )
                , ( "wrong", isNumberWrong model number ) -- Trying to get it to flash red when pressing the same wrong again
                ]
            ]
            [ Html.text (String.fromInt (number + 1)) ]
        ]


isNumberCorrect : Model -> Int -> Bool
isNumberCorrect model numberIndex =
    Set.member numberIndex (Set.map Tuple.second model.correctPairs)


isNumberWrong : Model -> Int -> Bool
isNumberWrong model number =
    case model.wrongPair of
        Just wrongPair ->
            Tuple.second wrongPair == number

        Nothing ->
            False


checkClickedValues : Model -> ( Model, Cmd Msg )
checkClickedValues model =
    case model.noteAndOriginalIndexTuplePrompted of
        Just ( originalIndex, _ ) ->
            if originalIndex == model.numberIndex then
                -- if correct
                let
                    newPair =
                        ( originalIndex, model.numberIndex )

                    newCorrectPairs =
                        Set.insert newPair model.correctPairs

                    nextExerciseStep =
                        model.exerciseStep + 1

                    newScore =
                        Set.size model.correctPairs

                    newNumberOfWins =
                        model.numberOfWins + 1

                    maybeNextNote =
                        model.chosenScale
                            |> Maybe.map (\scale -> List.drop nextExerciseStep scale.notes)
                            |> Maybe.andThen List.head

                    newModelAndCmd =
                        case maybeNextNote of
                            Just nextNote ->
                                ( { model
                                    | score = newScore
                                    , exerciseStep = nextExerciseStep
                                    , correctPairs = newCorrectPairs
                                    , noteAndOriginalIndexTuplePrompted = Just nextNote
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                -- when all notes are paired correctly (User Win)
                                ( { model
                                    | numberOfWins = newNumberOfWins
                                    , gameOver = True
                                    , result = Just Win
                                  }
                                , Cmd.batch [ saveWins newNumberOfWins ]
                                )
                in
                newModelAndCmd

            else
                -- if wrong
                let
                    newNumberOfWrongs =
                        model.numberOfWrongs + 1

                    newWrongPair =
                        ( originalIndex, model.numberIndex )

                    newModelAndCmd =
                        if newNumberOfWrongs == 3 then
                            -- if lose
                            ( { model | gameOver = True, result = Just Lose }, Cmd.none )

                        else
                            ( { model
                                | numberOfWrongs = newNumberOfWrongs
                                , wrongPair = Just newWrongPair
                              }
                            , Cmd.none
                            )
                in
                newModelAndCmd

        Nothing ->
            ( model, Cmd.none )


getOriginalIndexOfNotePrompted : Model -> Int
getOriginalIndexOfNotePrompted model =
    let
        originalIndex =
            case model.noteAndOriginalIndexTuplePrompted of
                Just noteAndIndexTuple ->
                    Tuple.first noteAndIndexTuple

                Nothing ->
                    7
    in
    originalIndex


setUserWins : Cmd Msg
setUserWins =
    Task.perform (\_ -> Reset True) (Process.sleep 0)
