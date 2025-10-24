module Games.Stopwatch exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Time exposing (Posix)


type alias Model =
    { running : Bool
    , stopwatchInMs : Int
    }


type Msg
    = Tick Posix
    | Start
    | Stop


init : Model
init =
    { running = False
    , stopwatchInMs = 0
    }



subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every 10 Tick
    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | stopwatchInMs = model.stopwatchInMs + 100 }, Cmd.none )

        Start ->
            ( { model | running = True, stopwatchInMs = model.stopwatchInMs + 10000}, Cmd.none )

        Stop ->
            ( { model | running = False }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        stopwatchInMsSplit =
            if model.stopwatchInMs == 0 then
                splitAt 1 <| String.fromFloat (toFloat model.stopwatchInMs / 100) ++ "00"

            else
                splitAt 1 <| String.fromFloat (toFloat model.stopwatchInMs / 100)
    in
    Html.div []
        [ Html.text <| stopwatchInMsSplit
        , if not model.running then
            Html.button [ HE.onClick Start ] [ Html.text "Start" ]

          else
            Html.button [ HE.onClick Stop ] [ Html.text "Stop" ]
        ]


splitAt : Int -> String -> String
splitAt index str =
    case ( String.left index str, String.dropLeft index str ) of
        ( leftSide, rightSide ) ->
            leftSide ++ "." ++ rightSide
