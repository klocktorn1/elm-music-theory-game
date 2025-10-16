module Game.ChordExercise exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type alias Model =
    { value : String
    }


type Msg
    = Test


type alias Flags =
    String


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { value = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Test ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [] [ Html.text "Chord exercise" ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
