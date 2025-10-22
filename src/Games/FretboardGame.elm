module Games.FretboardGame exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp)
import Html exposing (Html)
import Html.Events as HE
import Json.Decode as Decode
import Svg exposing (Svg, svg)
import Svg.Attributes as SVGA
import Time exposing (Posix)



-- MODEL


type alias Pos =
    { string : Float
    , fret : Float
    }


type alias Model =
    { targetNote : String
    , player : Pos
    , userPosition : ( Float, Float )
    , fretCount : Float
    , stringCount : Float
    }


type KeyUpOrDown
    = Up
    | Down


init : () -> ( Model, Cmd Msg )
init _ =
    ( { targetNote = "C"
      , player = { string = 1, fret = 0 }
      , userPosition = ( 24, 14 ) -- starting pixel position
      , fretCount = 12
      , stringCount = 6
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyDown String
    | KeyUp String
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            let
                newPos =
                    move key Down model.player model
            in
            ( { model | player = newPos }, Cmd.none )

        KeyUp key ->
            let
                newPos =
                    move key Up model.player model
            in
            ( { model | player = newPos }, Cmd.none )

        Tick _ ->
            let
                target =
                    posToXY model.player

                ( x, y ) =
                    model.userPosition

                ( tx, ty ) =
                    target

                newuserPosition =
                    ( lerp x tx 0.2, lerp y ty 0.2 )
            in
            ( { model | userPosition = newuserPosition }
            , Cmd.none
            )



-- MOVEMENT


move : String -> KeyUpOrDown -> Pos -> Model -> Pos
move key upOrDown pos model =
    case key of
        "ArrowLeft" ->
            if upOrDown == Down then
                { pos | fret = clamp 0 (model.fretCount - 1) (pos.fret - 1) }

            else
                { pos | fret = clamp 0 (model.fretCount - 1) (pos.fret - 0) }

        "ArrowRight" ->
            if upOrDown == Down then
                { pos | fret = clamp 0 (model.fretCount - 1) (pos.fret + 1) }

            else
                { pos | fret = clamp 0 (model.fretCount - 1) (pos.fret + 0) }

        "ArrowUp" ->
            if upOrDown == Down then
                { pos | string = clamp 1 model.stringCount (pos.string - 1) }

            else
                pos

        "ArrowDown" ->
            if upOrDown == Down then
                { pos | string = clamp 1 model.stringCount (pos.string + 1) }

            else
                pos

        _ ->
            pos



--keep within bounds


clamp : Int -> Float -> Float -> Float
clamp lo hi val =
    if val < toFloat lo then
        toFloat lo

    else if val > hi then
        hi

    else
        val


lerp : Float -> Float -> Float -> Float
lerp a b t =
    a + (b - a) * t



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text ("Find note: " ++ model.targetNote)]
        , Html.p [] [Html.text <| Debug.toString model.userPosition ]
        , svg
            [ SVGA.width "800", SVGA.height "200", SVGA.style "background:#f0f0f0" ]
            (drawFretboard model ++ [ drawPlayer model ])
        ]


drawFretboard : Model -> List (Svg Msg)
drawFretboard model =
    let
        stringSpacing =
            28

        fretWidth =
            48
    in
    List.concat
        [ -- frets
          List.map
            (\f ->
                Svg.line
                    (List.append
                    
                        [ SVGA.x1 (String.fromFloat (toFloat f * fretWidth))
                        , SVGA.x2 (String.fromFloat (toFloat f * fretWidth))
                        , SVGA.y1 "14"
                        , SVGA.y2 (String.fromFloat (model.stringCount * stringSpacing - 14))
                        ]
                        (if f == 1 then
                            [ SVGA.strokeWidth "10"
                            , SVGA.stroke "black"
                            ]

                         else
                            [ SVGA.strokeWidth "1"
                            , SVGA.stroke "#ccc"
                            ]
                        )
                    )
                    []
            )
            (List.range 0 (round model.fretCount))
        , -- strings
          List.map
            (\i ->
                Svg.line
                    [ SVGA.x1 "0"
                    , SVGA.x2 (String.fromFloat (model.fretCount * fretWidth))
                    , SVGA.y1 (String.fromFloat (toFloat (i - 1) * stringSpacing + stringSpacing / 2))
                    , SVGA.y2 (String.fromFloat (toFloat (i - 1) * stringSpacing + stringSpacing / 2))
                    , SVGA.stroke "black"
                    , SVGA.strokeWidth "1"
                    ]
                    []
            )
            (List.range 1 (round model.stringCount))
        ]





drawPlayer : Model -> Svg Msg
drawPlayer model =
    let
        ( x, y ) =
            model.userPosition
    in
    Svg.g [ SVGA.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")") ]
        [ Svg.circle [ SVGA.r "10", SVGA.fill "red" ] [] ]



-- HELPER: logical -> screen coords


posToXY : Pos -> ( Float, Float )
posToXY pos =
    let
        fretWidth =
            48

        stringSpacing =
            28
    in
    ( pos.fret * fretWidth + fretWidth / 2
    , (pos.string - 1) * stringSpacing + stringSpacing / 2
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map KeyDown keyDecoder)
        , onKeyUp (Decode.map KeyUp keyDecoder)
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
