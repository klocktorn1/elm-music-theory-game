module Games.TheoryApi exposing (Chord, MajorScale, Mode, Note, TheoryDb, buildErrorMessage, fetchTheoryDb)

import Http
import Json.Decode as Decode



type alias Note =
    String


type alias TheoryDb =
    { majorScales : List MajorScale
    , modes : List Mode
    , allNotes : List Note
    , chords : List Chord
    }


type alias Chord =
    { name : String
    , formula : List String
    }


type alias MajorScale =
    { key : String
    , notes : List ( Int, Note )
    }


type alias Mode =
    { mode : String
    , formula : List String
    }


theoryDbDecoder : Decode.Decoder TheoryDb
theoryDbDecoder =
    Decode.map4 TheoryDb
        (Decode.field "major-scales" (Decode.list majorScaleDecoder))
        (Decode.field "modes" (Decode.list modeDecoder))
        (Decode.field "all-notes" (Decode.list Decode.string))
        (Decode.field "chords" (Decode.list chordDecoder))


majorScaleDecoder : Decode.Decoder MajorScale
majorScaleDecoder =
    Decode.map2 MajorScale
        (Decode.field "key" Decode.string)
        (Decode.field "notes" (Decode.list Decode.string)
            |> Decode.map (List.indexedMap Tuple.pair)
        )


modeDecoder : Decode.Decoder Mode
modeDecoder =
    Decode.map2 Mode
        (Decode.field "mode" Decode.string)
        (Decode.field "formula" (Decode.list Decode.string))


chordDecoder : Decode.Decoder Chord
chordDecoder =
    Decode.map2 Chord
        (Decode.field "name" Decode.string)
        (Decode.field "formula" (Decode.list Decode.string))


fetchTheoryDb : (Result Http.Error TheoryDb -> msg) -> Cmd msg
fetchTheoryDb toMsg =
    Http.get
        { url = "http://localhost:5019/theory-db"
        , expect = Http.expectJson toMsg theoryDbDecoder
        }


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            "BadUrl: " ++ message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later"

        Http.NetworkError ->
            "Unable to reach server"

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ (statusCode |> String.fromInt)

        Http.BadBody message ->
            "BadBody: " ++ message
