module Game.ModeExercise exposing (..)

import Browser
import Game.NoteExercise exposing (Msg(..))
import Game.TheoryApi as TheoryApi
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http


type alias Model =
    { value : String
    , modes : Maybe TheoryApi.Modes
    , keys : Maybe (List TheoryApi.Key)
    , chosenKey : Maybe TheoryApi.Key
    , chosenMode : Maybe TheoryApi.Modes
    , modesErrorMessage : Maybe String
    , keysErrorMessage : Maybe String
    }


type Msg
    = ModesFetched (Result Http.Error TheoryApi.Modes)
    | ScalesFetched (Result Http.Error (List TheoryApi.Key))
    | ChooseKey TheoryApi.Key
    | ChooseMode TheoryApi.Modes


type alias Flags =
    String



{-
   todo friday: fetch scales from api and use the mode formulas to construct the modes.
   aeolian formula looks like this: [0,0,-1,0,0,-1,-1] and c major scale : [C, D, E, F, G, A, B,]
   so i need to create a function that takes each number from the formula list and adds a flat (b)
   to each note that should be flattened


   User chooses key ->
   random mode is applied to that key ->
   major scale with mode applied to is rendered on screen ->
   all seven modes are rendered on screen as buttons ->
   user clicks which mode they think is correct ->
   check if user is correct or not

-}


fetchScales : Cmd Msg
fetchScales =
    TheoryApi.fetchScales ScalesFetched


fetchModes : Cmd Msg
fetchModes =
    TheoryApi.fetchModes ModesFetched


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { value = ""
      , modes = Nothing
      , keys = Nothing
      , chosenKey = Nothing
      , chosenMode = Nothing
      , modesErrorMessage = Nothing
      , keysErrorMessage = Nothing
      }
    , Cmd.batch [ fetchScales, fetchModes ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModesFetched (Ok modes) ->
            ( { model | modes = Just modes }, Cmd.none )

        ModesFetched (Err httpError) ->
            ( { model | modesErrorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

        ScalesFetched (Ok keys) ->
            ( { model | keys = Just keys }, Cmd.none )

        ScalesFetched (Err httpError) ->
            ( { model | keysErrorMessage = Just (buildErrorMessage httpError) }, Cmd.none )

        ChooseKey key ->
            let
                newModel =
                    { model
                        | chosenKey = Just key
                    }
            in
            ( newModel, Cmd.none )

        ChooseMode mode ->
            ( { model | chosenMode = Just mode }, Cmd.none )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later"

        Http.NetworkError ->
            "Unable to reach server"

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ (statusCode |> String.fromInt)

        Http.BadBody message ->
            message


view : Model -> Html Msg
view model =
    Html.div []
        [ viewKeysOrError model
        , viewScale model
        ]


viewScale : Model -> Html Msg
viewScale model =
    case model.chosenKey of
        Just chosenKey ->
            Html.div [] [ viewConstructedMode chosenKey]
        Nothing ->
            Html.div [] [Html.text "Please choose a key"]



viewConstructedMode : TheoryApi.Key -> Html Msg
viewConstructedMode chosenKey =
    Html.div [] 
        [
            Html.div []
                         ((chosenKey
                             |> notesToListString
                             |> constructScale chosenKey
                             |> List.map viewConstructedScale
                          )
                             ++ [ Html.text (Debug.toString (List.map String.length (notesToListString chosenKey))) ]
                         )
        ]


viewKeysOrError : Model -> Html Msg
viewKeysOrError model =
    case model.keysErrorMessage of
        Just error ->
            Html.div []
                [ Html.text error ]

        Nothing ->
            case model.keys of
                Just keys ->
                    Html.div [ HA.class "key-buttons-container" ] (List.map viewKeyButtons keys)

                Nothing ->
                    Html.div [] [ Html.text "something else" ]


viewKeyButtons : TheoryApi.Key -> Html Msg
viewKeyButtons key =
    Html.div []
        [ Html.div [ HA.class "key-button", HE.onClick (ChooseKey key) ]
            [ Html.text key.key
            ]
        ]


viewConstructedScale : String -> Html Msg
viewConstructedScale constructedNote =
    Html.div []
        [ Html.text constructedNote
        , Html.div [] []
        ]


viewModes : Maybe TheoryApi.Modes -> TheoryApi.Key -> Html Msg
viewModes maybeModes key =
    case maybeModes of
        Just modes ->
            Html.div [ HA.class "key-button" ] (List.map viewModeButtons modes)

        Nothing ->
            Html.div [] [ Html.text "something else" ]


viewModeButtons : TheoryApi.Mode -> Html Msg
viewModeButtons mode =
    Html.div [ HA.class "key-button" ] [ Html.text mode.mode ]


notesToListString : TheoryApi.Key -> List String
notesToListString key =
    List.map Tuple.second key.notes


constructScale : List String -> List String -> List String
constructScale mode key =
    List.map2 (++) key mode
        |> List.map (String.replace "#b" "")
        |> List.map (String.replace "b#" "")


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



{-

         viewScaleOrError : Model -> Html Msg
         viewScaleOrError model =
             case ( model.chosenKey, model.modes ) of
                 ( Just chosenKey, Just modes ) ->
                     Html.div []
                         ((chosenKey
                             |> notesToListString
                             |> constructScale modes.formula
                             |> List.map viewConstructedScale
                          )
                             ++ [ Html.text (Debug.toString (List.map String.length (notesToListString chosenKey))) ]
                         )

                 -- (List.map viewConstructedScale (constructScale (notesToListString chosenKey) modes.aeolian)) <-- i find this easier to understand
                 _ ->
                     Html.div [] [ Html.text "viewScaleOrError Nothing" ]


      viewKeys : Maybe (List TheoryApi.Key) -> Html Msg
      viewKeys keys =
          case keys of
              Just allKeys ->
                  Html.div [ HA.class "key-buttons-container" ] (List.map viewKeyButtons allKeys)

              Nothing ->
                  Html.div [] [ Html.text "something else" ]


      viewKeyButtons : TheoryApi.Key -> Html Msg
      viewKeyButtons key =
          Html.div []
              [ Html.div [ HA.class "key-button", HE.onClick (ChooseKey key) ]
                  [ Html.text key.key
                  ]
              ]


   viewKeysOrError : Model -> Html Msg
   viewKeysOrError model =
       case model.scalesErrorMessage of
           Just error ->
               Html.div []
                   [ Html.text error ]

           Nothing ->
               Html.div []
                   [ viewKeys model.scales ]




-}
