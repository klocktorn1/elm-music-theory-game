module Games.ChordExercise exposing (..)

import Array
import Games.TheoryApi as TheoryApi
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List.Extra as ListExtra
import Random


type alias Model =
    { maybeChords : Maybe (List TheoryApi.Chord)
    , maybeMajorScalesAndKeys : Maybe (List TheoryApi.MajorScaleAndKey)
    , chosenKeyAndScale : Maybe TheoryApi.MajorScaleAndKey
    , maybeChosenChord : Maybe TheoryApi.Chord
    , randomizedChord : Maybe TheoryApi.Chord
    , lastRandomIndex : Maybe Int
    , constructedRandomizedChord : List String
    , constructedChosenChord : List String
    , score : Int
    , mistakes : Int
    , gameOver : Bool
    }


type Msg
    = KeyAndScaleChosen TheoryApi.MajorScaleAndKey
    | GotTheoryDb TheoryApi.TheoryDb
    | RandomChordPicked Int
    | ChordChosen TheoryApi.Chord
    | Reset


type alias Flags =
    String


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { maybeChords = Nothing
      , maybeMajorScalesAndKeys = Nothing
      , chosenKeyAndScale = Nothing
      , maybeChosenChord = Nothing
      , randomizedChord = Nothing
      , lastRandomIndex = Nothing
      , constructedRandomizedChord = []
      , constructedChosenChord = []
      , score = 0
      , mistakes = 0
      , gameOver = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyAndScaleChosen key ->
            ( { model | chosenKeyAndScale = Just key }, randomizeChord )

        GotTheoryDb db ->
            ( { model
                | maybeMajorScalesAndKeys = Just db.majorScalesAndKeys
                , maybeChords = Just db.chords
              }
            , Cmd.none
            )

        RandomChordPicked randomIndex ->
            case model.lastRandomIndex of
                Just lastRandomIndex ->
                    if lastRandomIndex == randomIndex then
                        ( model, randomizeChord )

                    else
                        let
                            newRandomizedChord =
                                pickRandomChord model.maybeChords randomIndex
                        in
                        ( { model | randomizedChord = Just newRandomizedChord, lastRandomIndex = Just randomIndex }, Cmd.none )

                Nothing ->
                    let
                        newRandomizedChord =
                            pickRandomChord model.maybeChords randomIndex
                    in
                    ( { model | randomizedChord = Just newRandomizedChord, lastRandomIndex = Just randomIndex }, Cmd.none )

        ChordChosen chord ->
            let
                modelWithChordChosen =
                    { model | maybeChosenChord = Just chord }

                modelWithCheckedValues =
                    checkIfChordIsCorrect modelWithChordChosen
            in
            modelWithCheckedValues

        Reset ->
            ( { model
                | maybeChords = Nothing
                , maybeChosenChord = Nothing
                , gameOver = False
                , mistakes = 0
                , score = 0
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.maybeMajorScalesAndKeys of
        Just majorScalesAndKeys ->
            Html.div []
                [ Html.text "Chord exercise"
                , Html.div [ HA.class "key-buttons-container" ]
                    (List.map viewKeys majorScalesAndKeys)
                , viewGameStarted model
                ]

        Nothing ->
            Html.div [] [ Html.text "Something went wrong" ]


viewGameStarted : Model -> Html Msg
viewGameStarted model =
    case model.chosenKeyAndScale of
        Just chosenKeyAndScale ->
            if not model.gameOver then
                Html.div []
                    [ viewRandomizedChord model
                    , viewChords model chosenKeyAndScale
                    , Html.p [] [ Html.text <| "Score:  " ++ String.fromInt model.score ]
                    , Html.p [] [ Html.text <| "Mistakes:  " ++ String.fromInt model.mistakes ]
                    ]

            else
                viewGameOverMessage model

        Nothing ->
            Html.div [] [ Html.text "Please choose a key" ]


viewKeys : TheoryApi.MajorScaleAndKey -> Html Msg
viewKeys majorScaleAndKey =
    Html.button
        [ HA.class "custom-button"
        , HE.onClick (KeyAndScaleChosen majorScaleAndKey)
        ]
        [ Html.text majorScaleAndKey.key ]


viewChords : Model -> TheoryApi.MajorScaleAndKey -> Html Msg
viewChords model chosenKeyAndScale =
    case model.maybeChords of
        Just chords ->
            Html.div [ HA.class "chords-container" ]
                (List.map (viewChord chosenKeyAndScale) chords) 

        Nothing ->
            Html.div [] [ Html.text "No chords found" ]


viewChord : TheoryApi.MajorScaleAndKey -> TheoryApi.Chord -> Html Msg
viewChord chosenKeyAndScale chord =
    Html.div [ HA.class "custom-button", HE.onClick (ChordChosen chord) ]
        [ Html.text (chosenKeyAndScale.key ++ chord.name)
        ]


viewRandomizedChord : Model -> Html Msg
viewRandomizedChord model =
    case model.randomizedChord of
        Just randomizedChord ->
            Html.p [] [ Html.text ("Which chord is this? " ++ chordConstructor randomizedChord model.chosenKeyAndScale) ]

        Nothing ->
            Html.p [] [ Html.text "No chord found" ]


viewGameOverMessage : Model -> Html Msg
viewGameOverMessage model =
    Html.div
        [ HA.class "game-over-message-container"
        , if not model.gameOver then
            HA.style "display" "none"

          else
            HA.style "display" ""
        ]
        [ Html.p [] [ Html.text "You lose" ]
        , Html.p []
            [ Html.button [ HE.onClick Reset, HA.class "custom-button" ] [ Html.text "Try again" ] ]
        ]


listTuplesToListString : List ( Int, String ) -> List String
listTuplesToListString listOfTuples =
    List.map Tuple.second listOfTuples


chordConstructor : TheoryApi.Chord -> Maybe TheoryApi.MajorScaleAndKey -> String
chordConstructor chord maybeMajorScaleAndKey =
    case maybeMajorScaleAndKey of
        Just majorScaleAndKey ->
            List.map2 (++) (ListExtra.removeIfIndex (\index -> modBy 2 index /= 0) (listTuplesToListString majorScaleAndKey.notes)) chord.formula
                |> List.map (String.replace "#b" "")
                |> List.map (String.replace "b#" "")
                |> String.join " "

        Nothing ->
            "Please choose a key"


pickRandomChord : Maybe (List TheoryApi.Chord) -> Int -> TheoryApi.Chord
pickRandomChord maybeChords randomIndex =
    case maybeChords of
        Just chords ->
            let
                maybeRandomChordObject =
                    case Array.get randomIndex (Array.fromList chords) of
                        Just randomChordObject ->
                            randomChordObject

                        Nothing ->
                            { name = "No chord found at given index", formula = [] }
            in
            maybeRandomChordObject

        Nothing ->
            { name = "No chords found", formula = [] }


randomizeChord : Cmd Msg
randomizeChord =
    Random.generate RandomChordPicked (Random.int 0 3)


checkIfChordIsCorrect : Model -> ( Model, Cmd Msg )
checkIfChordIsCorrect model =
    case ( model.maybeChosenChord, model.randomizedChord ) of
        ( Just chosenChord, Just randomizedChord ) ->
            if chosenChord == randomizedChord then
                let
                    _ =
                        Debug.log "Win!" "Win"

                    newScore =
                        model.score + 1
                in
                ( { model | score = newScore }, randomizeChord )

            else
                let
                    newMistakes =
                        model.mistakes + 1

                    setGameOver =
                        if model.mistakes == 2 then
                            True

                        else
                            False
                in
                ( { model | mistakes = newMistakes, gameOver = setGameOver }, Cmd.none )

        _ ->
            ( model, Cmd.none )
