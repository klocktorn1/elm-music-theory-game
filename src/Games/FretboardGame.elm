module Games.FretboardGame exposing (..)
import Browser
import Html exposing (Html, div, text)
import Html.Events exposing (on)
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes as SVGA
import Time exposing (Posix)
import Browser.Events exposing (onAnimationFrame, onKeyDown)


-- MODEL

type alias Pos =
    { string : Int
    , fret : Int
    }

type alias Model =
    { targetNote : String
    , player : Pos
    , display : ( Float, Float )
    , fretCount : Int
    , stringCount : Int
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { targetNote = "C"
      , player = { string = 1, fret = 0 }
      , display = ( 24, 14 ) -- starting pixel position
      , fretCount = 12
      , stringCount = 6
      }
    , Cmd.none
    )


-- UPDATE

type Msg
    = KeyDown String
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            let
                newPos =
                    move key model.player model
            in
            ( { model | player = newPos }, Cmd.none )

        Tick _ ->
            let
                target = posToXY model.player
                ( x, y ) = model.display
                ( tx, ty ) = target
                newDisplay =
                    ( lerp x tx 0.2, lerp y ty 0.2 )
            in
            ( { model | display = newDisplay }
            , Cmd.none
            )


-- MOVEMENT

move : String -> Pos -> Model -> Pos
move key pos model =
    case key of
        "ArrowLeft" ->
            { pos | fret = clamp 0 (model.fretCount - 1) (pos.fret - 1) }

        "ArrowRight" ->
            { pos | fret = clamp 0 (model.fretCount - 1) (pos.fret + 1) }

        "ArrowUp" ->
            { pos | string = clamp 1 model.stringCount (pos.string - 1) }

        "ArrowDown" ->
            { pos | string = clamp 1 model.stringCount (pos.string + 1) }

        _ ->
            pos


clamp : Int -> Int -> Int -> Int
clamp lo hi val =
    if val < lo then lo else if val > hi then hi else val


lerp : Float -> Float -> Float -> Float
lerp a b t =
    a + (b - a) * t


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div [] [ Html.text ("Find note: " ++ model.targetNote) ]
        , svg
            [ SVGA.width "800", SVGA.height "200", SVGA.style "background:#f0f0f0" ]
            (drawFretboard model ++ [ drawPlayer model ])
        ]


drawFretboard : Model -> List (Svg Msg)
drawFretboard model =
    let
        stringSpacing = 28
        fretWidth = 48
    in
    List.concat
        [ -- strings
          List.map
            (\i ->
                line
                    [ SVGA.x1 "0"
                    , SVGA.x2 (String.fromFloat (toFloat model.fretCount * fretWidth))
                    , SVGA.y1 (String.fromFloat (toFloat (i - 1) * stringSpacing + stringSpacing / 2))
                    , SVGA.y2 (String.fromFloat (toFloat (i - 1) * stringSpacing + stringSpacing / 2))
                    , SVGA.stroke "black"
                    , SVGA.strokeWidth "1"
                    ]
                    []
            )
            (List.range 1 model.stringCount)
        , -- frets
          List.map
            (\f ->
                line
                    [ SVGA.x1 (String.fromFloat (toFloat f * fretWidth))
                    , SVGA.x2 (String.fromFloat (toFloat f * fretWidth))
                    , SVGA.y1 "0"
                    , SVGA.y2 (String.fromFloat (toFloat model.stringCount * stringSpacing))
                    , SVGA.stroke "#ccc"
                    ]
                    []
            )
            (List.range 0 model.fretCount)
        ]


drawPlayer : Model -> Svg Msg
drawPlayer model =
    let
        ( x, y ) = model.display
    in
    g [ SVGA.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")") ]
        [ circle [ SVGA.r "10", SVGA.fill "red" ] [] ]


-- HELPER: logical -> screen coords

posToXY : Pos -> ( Float, Float )
posToXY pos =
    let
        fretWidth = 48
        stringSpacing = 28
    in
    ( toFloat pos.fret * fretWidth + fretWidth / 2
    , toFloat (pos.string - 1) * stringSpacing + stringSpacing / 2
    )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map KeyDown keyDecoder)
        , onAnimationFrame Tick
        ]



keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

        
