module Games.ChordExercise exposing (..)

import Games.TheoryApi as TheoryApi
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List.Extra as ListExtra


type alias Model =
    { maybeChords : Maybe (List TheoryApi.Chord)
    , maybeMajorScalesAndKeys : Maybe (List TheoryApi.MajorScaleAndKey)
    , chosenKey : Maybe String
    }


type Msg
    = ChooseKey String
    | GotTheoryDb TheoryApi.TheoryDb


type alias Flags =
    String


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { maybeChords = Nothing
      , maybeMajorScalesAndKeys = Nothing
      , chosenKey = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseKey key ->
            ( { model | chosenKey = Just key }, Cmd.none )

        GotTheoryDb db ->
            let
                _ =
                    Debug.log "" db
            in
            ( { model | maybeMajorScalesAndKeys = Just db.majorScalesAndKeys }, Cmd.none )



view : Model -> Html Msg
view model =
    case model.maybeMajorScalesAndKeys of
        Just majorScalesAndKeys ->
            Html.div []
                [ Html.text "Chord exercise"
                , Html.div [ HA.class "key-buttons-container" ]
                    (List.map viewKeys majorScalesAndKeys)
                ]

        Nothing ->
            Html.div [] [ Html.text "Please choose a key"]


viewKeys : TheoryApi.MajorScaleAndKey -> Html Msg
viewKeys majorScaleAndKey =
    Html.button [ HA.class "custom-button" ] [ Html.text majorScaleAndKey.key ]


chordConstructor : List String -> List String
chordConstructor majorScaleAndKey =
    ListExtra.removeIfIndex (\index -> modBy 2 index == 0) majorScaleAndKey
