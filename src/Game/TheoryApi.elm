module Game.TheoryApi exposing (Modes, Note, Key, Mode, fetchModes, fetchScales, scaleDecoder)

import Http
import Json.Decode as Decode


type alias Note =
    String


type alias Key =
    { key : String
    , notes : List ( Int, Note )
    }


type alias Modes =
    List Mode


type alias Mode =
    { mode : String
    , formula : List String
    }


scaleDecoder : Decode.Decoder Key
scaleDecoder =
    Decode.map2 Key
        (Decode.field "key" Decode.string)
        (Decode.field "notes" (Decode.list Decode.string)
            |> Decode.map (List.indexedMap Tuple.pair)
        )


modeDecoder : Decode.Decoder Mode
modeDecoder =
    Decode.map2 Mode
        (Decode.field "mode" Decode.string)
        (Decode.field "formula" (Decode.list Decode.string))
    



{-

type Foo
    = A { x : Int }
    | B { y : String }


decodeFoo : Decode.Decoder Foo
decodeFoo =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "A" ->
                        Decode.map
                            (\x -> A { x = x })
                            (Decode.field "x" Decode.int)

                    _ ->
                        Decode.fail "Unkown tag"
            )


-}


fetchScales : (Result Http.Error (List Key) -> msg) -> Cmd msg
fetchScales toMsg =
    Http.get
        { url = "http://localhost:5019/scales"
        , expect = Http.expectJson toMsg (Decode.list scaleDecoder)
        }


fetchModes : (Result Http.Error Modes -> msg) -> Cmd msg
fetchModes toMsg =
    Http.get
        { url = "http://localhost:5019/modes"
        , expect = Http.expectJson toMsg (Decode.list modeDecoder)
        }
