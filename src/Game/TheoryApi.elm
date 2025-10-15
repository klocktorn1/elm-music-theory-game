module Game.TheoryApi exposing (MajorScale, Mode, Modes, Note, TheoryDb, fetchTheoryDb, buildErrorMessage)

import Http
import Json.Decode as Decode


type alias Note =
    String


type alias TheoryDb =
    { majorScales : List MajorScale
    , modes : List Mode
    , allNotes : List Note
    }


type alias MajorScale =
    { key : String
    , notes : List ( Int, Note )
    }


type alias Modes =
    List Mode


type alias Mode =
    { mode : String
    , formula : List String
    }


theoryDbDecoder : Decode.Decoder TheoryDb
theoryDbDecoder =
    Decode.map3 TheoryDb
        (Decode.field "major-scales" (Decode.list majorScaleDecoder))
        (Decode.field "modes" (Decode.list modeDecoder))
        (Decode.field "all-notes" (Decode.list Decode.string))


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
{-

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


   fetchAllNotes : (Result Http.Error AllNotes -> msg) -> Cmd msg
   fetchAllNotes toMsg =
       Http.get
           { url = "http://localhost:5019/all-notes"
           , expect = Http.expectJson toMsg allNotesDecoder
           }


-}
