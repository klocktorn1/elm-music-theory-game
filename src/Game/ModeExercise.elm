module Game.ModeExercise exposing (..)

import Array
import Browser
import Game.NoteExercise exposing (Msg(..))
import Game.TheoryApi as TheoryApi
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Random


type alias Model =
    { value : String
    , modes : Maybe TheoryApi.Modes
    , keys : Maybe (List TheoryApi.Key)
    , chosenKey : Maybe TheoryApi.Key
    , randomizedMode : TheoryApi.Mode
    , modesErrorMessage : Maybe String
    , keysErrorMessage : Maybe String
    , modeGuessed : Maybe String
    , isCorrect : Bool
    , score : Int
    , resultMessage : String
    }


type Msg
    = ModesFetched (Result Http.Error TheoryApi.Modes)
    | ScalesFetched (Result Http.Error (List TheoryApi.Key))
    | ChooseKey TheoryApi.Key
    | PickRandomMode Int
    | ModeGuessed String


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
      , randomizedMode = { mode = "No mode randomized yet", formula = [] }
      , modesErrorMessage = Nothing
      , keysErrorMessage = Nothing
      , modeGuessed = Nothing
      , isCorrect = False
      , score = 0
      , resultMessage = ""
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
                        , resultMessage = ""
                    }
            in
            ( newModel, randomizeMode )

        PickRandomMode randomIndex ->
            ( { model | randomizedMode = pickRandomMode model.modes randomIndex }, Cmd.none )

        ModeGuessed mode ->
            let
                modelWithModeGuessed =
                    { model | modeGuessed = Just mode }

                modelWithResult =
                    checkIfCorrect modelWithModeGuessed
            in
            modelWithResult


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


pickRandomMode : Maybe TheoryApi.Modes -> Int -> TheoryApi.Mode
pickRandomMode maybeModes randomIndex =
    case maybeModes of
        Just modes ->
            let
                maybeRandomModeObject =
                    case Array.get randomIndex (Array.fromList modes) of
                        Just randomModeObject ->
                            randomModeObject

                        Nothing ->
                            { mode = "No mode found at given index", formula = [] }
            in
            maybeRandomModeObject

        Nothing ->
            { mode = "No mode found", formula = [] }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text """Choose a key below. A randomized mode 
                        (ionian, dorian, phrygian, lydian, mixolydian, aeolian or locrian) 
                        will be applied to the chosen key. Choose which mode is correct."""
        , viewKeysOrError model
        , viewScale model
        ]


viewScale : Model -> Html Msg
viewScale model =
    case model.chosenKey of
        Just chosenKey ->
            Html.div []
                [ viewConstructedMode chosenKey
                    model.randomizedMode.formula
                , viewModes model
                , Html.p [] [ Html.text ("Score: " ++ String.fromInt model.score) ]
                , Html.text model.resultMessage
                , Html.p [] [ Html.text ("For debugging, correct answer is: " ++ model.randomizedMode.mode) ]
                ]

        Nothing ->
            Html.div [] [ Html.text "Please choose a key" ]


viewModes : Model -> Html Msg
viewModes model =
    case model.modes of
        Just modes ->
            Html.div [] (List.map viewModeButton modes)

        Nothing ->
            Html.div [] []


viewModeButton : TheoryApi.Mode -> Html Msg
viewModeButton mode =
    Html.button [ HE.onClick (ModeGuessed mode.mode), HA.class "key-button", HA.style "margin-right" "20px" ] [ Html.text mode.mode ]


viewConstructedMode : TheoryApi.Key -> List String -> Html Msg
viewConstructedMode chosenKey mode =
    Html.div []
        [ Html.div []
            (chosenKey
                |> notesToListString
                |> constructScale mode
                |> List.map viewConstructedScale
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


checkIfCorrect : Model -> ( Model, Cmd Msg )
checkIfCorrect model =
    if model.modeGuessed == Just model.randomizedMode.mode then
        ( { model | score = model.score + 1, isCorrect = True, resultMessage = "Correct!" }, randomizeMode )

    else
        ( { model | resultMessage = "Wrong, try again." }, Cmd.none )


randomizeMode : Cmd Msg
randomizeMode =
    Random.generate PickRandomMode (Random.int 0 6)


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
