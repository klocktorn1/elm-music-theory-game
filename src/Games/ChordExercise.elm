module Games.ChordExercise exposing (..)

import Browser
import Games.TheoryApi as TheoryApi
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import List.Extra as ListExtra


type alias Model =
    { chords : Maybe (List TheoryApi.Chord)
    , majorScales : Maybe (List TheoryApi.MajorScale)
    , chosenKey : Maybe String
    }


type Msg
    = TheoryDbFetched (Result Http.Error TheoryApi.TheoryDb)
    | ChooseKey String


type alias Flags =
    String


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { chords = Nothing
      , majorScales = Nothing
      , chosenKey = Nothing
      }
    , TheoryApi.fetchTheoryDb TheoryDbFetched
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TheoryDbFetched (Ok theoryDb) ->
            ( { model
                | chords = Just theoryDb.chords
                , majorScales = Just theoryDb.majorScales
              }
            , Cmd.none
            )

        TheoryDbFetched (Err httpError) ->
            ( model, Cmd.none )

        ChooseKey key ->
            ( { model | chosenKey = Just key }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.majorScales of
        Just majorScales ->
            Html.div []
                [ Html.text "Chord exercise"
                , Html.div [ HA.class "key-buttons-container" ]
                    (List.map viewKeys majorScales)
                ]

        Nothing ->
            Html.div [] [ Html.text "Please choose a key" ]


viewKeys : TheoryApi.MajorScale -> Html Msg
viewKeys majorScale =
    Html.button [HA.class "custom-button"] [ Html.text majorScale.key ]


chordConstructor : List String -> List String
chordConstructor majorScale =
    ListExtra.removeIfIndex (\index -> modBy 2 index == 0) majorScale


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
