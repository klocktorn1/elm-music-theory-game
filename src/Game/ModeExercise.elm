module Game.ModeExercise exposing (..)

import Browser
import Html exposing (Html)
import Http
import Json.Decode as Decode


type alias Model =
    { value : String
    , modes : Maybe Modes
    }


type Msg
    = ModesFetched (Result Http.Error Modes)


type alias Flags =
    String


type alias Modes =
    { ionian : List Int
    , dorian : List Int
    , phrygian : List Int
    , lydian : List Int
    , mixolydian : List Int
    , aeolian : List Int
    , locrian : List Int
    }


{-
todo friday: fetch scales from api and use the mode formulas to construct the modes. 
aeolian formula looks like this: [0,0,-1,0,0,-1,-1] and c major scale : [C, D, E, F, G, A, B,]
so i need to create a function that takes each number from the formula list and adds a flat (b)
to each note that should be flattened


also add to github
                

-}


modeDecoder : Decode.Decoder Modes
modeDecoder =
    Decode.map7 Modes
        (Decode.field "ionian" (Decode.list Decode.int))
        (Decode.field "dorian" (Decode.list Decode.int))
        (Decode.field "phrygian" (Decode.list Decode.int))
        (Decode.field "lydian" (Decode.list Decode.int))
        (Decode.field "mixolydian" (Decode.list Decode.int))
        (Decode.field "aeolian" (Decode.list Decode.int))
        (Decode.field "locrian" (Decode.list Decode.int))


fetchModes : Cmd Msg
fetchModes =
    Http.get
        { url = " http://localhost:5019/modes"
        , expect = Http.expectJson ModesFetched modeDecoder
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { value = ""
      , modes = Nothing
      }
    , fetchModes
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModesFetched (Ok modes) ->
            ( { model | modes = Just modes }, Cmd.none )

        ModesFetched (Err error) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ viewModesOrError model.modes
        ]


viewModesOrError : Maybe Modes -> Html Msg
viewModesOrError maybeModes =
    case maybeModes of
        Just modes ->
            Html.div [] 
                [ viewModes modes ]

        Nothing ->
            Html.div [] []

viewModes : Modes -> Html Msg
viewModes modes =
    Html.div [] [Html.text (Debug.toString modes.aeolian)]

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
