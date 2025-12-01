module Games.NoteBuilder exposing (viewNotes)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List.Extra as ListExtra





viewNotes : (String -> msg) -> Maybe (List String) -> String -> Html msg
viewNotes toMsg maybeAllNotes chosenKey =
    case maybeAllNotes of
        Just allNotes ->
            Html.div []
                [ Html.div [ HA.class "note-builder-container" ] (List.map (viewNoteButtons toMsg) (viewNotesWithSharps (rotateList chosenKey allNotes)))
                , Html.div [ HA.class "note-builder-container" ] (List.map (viewNoteButtons toMsg) (viewNotesWithoutAccidentals (rotateList chosenKey allNotes)))
                , Html.div [ HA.class "note-builder-container" ] (List.map (viewNoteButtons toMsg) (viewNotesWithFlats (rotateList chosenKey allNotes)))
                ]

        Nothing ->
            Html.div [] [ Html.text "No notes" ]


viewNotesWithSharps : List String -> List String
viewNotesWithSharps allNotes =
    List.filter (\note -> String.slice 1 2 note == "#") allNotes


viewNotesWithoutAccidentals : List String -> List String
viewNotesWithoutAccidentals allNotes =
    List.filter (\note -> String.length note == 1) allNotes


viewNotesWithFlats : List String -> List String
viewNotesWithFlats allNotes =
    List.filter (\note -> String.slice 1 2 note == "b") allNotes


viewNoteButtons : (String -> msg) -> String -> Html msg
viewNoteButtons toMsg note =
    Html.button [ HA.class "custom-button", HE.onClick (toMsg note) ]
        [ Html.text note ]


rotateList : String -> List String -> List String
rotateList note allNotes =
    case ListExtra.findIndex (\noteFromList -> note == noteFromList) allNotes of
        Just index ->
            let
                ( start, end ) =
                    ListExtra.splitAt index allNotes
            in
            end ++ start

        Nothing ->
            []
