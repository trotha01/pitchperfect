port module Main exposing (Model, Msg(..), Note, init, initialModel, main, playSound, subscriptions, update, view, viewLink)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Images
import Process
import Radar exposing (..)
import Random
import Svg
import Svg.Attributes as SvgAttr exposing (attributeName, d, from, to, x, y)
import Task
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { -- navigation
      key : Nav.Key
    , url : Url.Url

    -- app
    , homescreen : Bool
    , points : Int
    , currentKey : Note
    , keysTesting : List Note
    , keysToTest : List Note
    , answer : Maybe ( Note, Bool )
    , currentKnowledge : Dict Note Knowledge
    , winner : Bool
    , playingSound : Bool

    -- randomness
    , seed : Random.Seed
    }


type alias Note =
    String


type alias Knowledge =
    Float


initialModel key url =
    { -- navigation
      key = key
    , url = url

    -- app
    , homescreen = True
    , points = 0
    , currentKey = ""
    , keysTesting = []
    , keysToTest = [ "C", "D", "E", "F", "G", "A", "B" ]
    , answer = Nothing
    , currentKnowledge = Dict.empty
    , winner = False
    , playingSound = False

    -- randomness
    , seed = Random.initialSeed 0
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( initialModel key url, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | StartGame
    | PlaySound Note
    | DonePlayingSound
    | Answer Note
    | NextSound
    | NextLevel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- NAVIGATION
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        -- APP
        StartGame ->
            { model | homescreen = False }
                |> update NextLevel

        PlaySound note ->
            ( { model | playingSound = True }
            , Cmd.batch
                [ playSound note
                , after 1000 DonePlayingSound
                ]
            )

        DonePlayingSound ->
            ( { model | playingSound = False }, Cmd.none )

        Answer note ->
            if note == model.currentKey then
                ( { model
                    | points = model.points + 100
                    , answer = Just ( note, True )
                    , currentKnowledge = Dict.update note increaseKnowledge model.currentKnowledge
                  }
                , Cmd.batch
                    [ playSound note
                    , after 1000 NextLevel
                    ]
                )

            else
                ( { model
                    | currentKnowledge = Dict.update note decreaseKnowledge model.currentKnowledge
                    , answer = Just ( note, False )
                  }
                , playSound note
                )

        NextSound ->
            let
                ( nextNote, nextSeed ) =
                    Random.step (randomNoteGenerator model) model.seed
            in
            ( { model
                | answer = Nothing
                , seed = nextSeed
                , currentKey = nextNote
              }
            , playSound nextNote
            )

        NextLevel ->
            if readyForNextLevel model.keysTesting model.currentKnowledge then
                let
                    newModel =
                        { model | answer = Nothing }
                            |> nextKey
                in
                update NextSound newModel

            else
                update NextSound model


randomNoteGenerator : Model -> Random.Generator Note
randomNoteGenerator model =
    let
        knowledgeList =
            Dict.toList model.currentKnowledge

        weightedKnowledge =
            List.map (\( note, knowledge ) -> ( weighKnowledge knowledge, note )) knowledgeList

        head =
            List.head weightedKnowledge |> Maybe.withDefault ( 1, "C" )

        tail =
            List.tail weightedKnowledge |> Maybe.withDefault [ ( 1, "C" ) ]
    in
    Random.weighted head tail


weighKnowledge : Knowledge -> Float
weighKnowledge knowledge =
    if knowledge == 0 then
        2

    else
        1 / knowledge


readyForNextLevel : List Note -> Dict Note Knowledge -> Bool
readyForNextLevel keysTesting currentKnowledge =
    let
        knowledgeNeeded =
            List.length keysTesting |> toFloat
    in
    Dict.foldl (\note knowledge ready -> ready && knowledge >= knowledgeNeeded) True currentKnowledge


increaseKnowledge : Maybe Knowledge -> Maybe Knowledge
increaseKnowledge knowledge =
    case knowledge of
        Nothing ->
            Just 1

        Just v ->
            Just (v + 1)


decreaseKnowledge : Maybe Knowledge -> Maybe Knowledge
decreaseKnowledge knowledge =
    case knowledge of
        Nothing ->
            Just 0

        Just v ->
            Just (v - 1)


nextKey : Model -> Model
nextKey model =
    let
        ( nextNote, keysRemaining ) =
            case List.head model.keysToTest of
                Just key ->
                    ( Just key, List.tail model.keysToTest |> Maybe.withDefault model.keysToTest )

                Nothing ->
                    -- this should never happen
                    ( Nothing, model.keysToTest )
    in
    case nextNote of
        Nothing ->
            -- YOU WIN!!!
            { model | winner = True }

        Just newKey ->
            { model
                | homescreen = False
                , currentKey = newKey
                , currentKnowledge = Dict.insert newKey 0 model.currentKnowledge
                , keysTesting = newKey :: model.keysTesting
                , keysToTest = keysRemaining
            }



{--| Run msg after a certain amount of milliseconds
  --}


after : Float -> msg -> Cmd msg
after time msg =
    Process.sleep time |> Task.perform (always msg)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- PORTS


port playSound : Note -> Cmd msg



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Pitch Perfect"
    , body =
        if model.homescreen then
            [ viewHomescreen model ]

        else
            [ viewGame model ]
    }


viewHomescreen : Model -> Html.Html Msg
viewHomescreen model =
    div [ class "column center homescreen" ]
        [ div [ class "header" ]
            [ h1 [] [ text "Pitch Perfect" ]
            ]
        , button [ class "start-button button", onClick StartGame ]
            [ i [ class "fas fa-play" ] [] ]

        -- , Images.view Images.soundWave
        -- , Images.view Images.c
        ]


viewGame : Model -> Html.Html Msg
viewGame model =
    div [ class "column game" ]
        [ viewHeader model
        , viewChoices model
        ]


viewHeader model =
    div [ class "row header" ]
        [ div [ class "title row grow-2" ]
            [ h1 [] [ text "Pitch Perfect" ]
            ]
        , div [ class "winner row grow" ]
            [ text <|
                if model.winner then
                    "PERFFECT PITCH!"

                else
                    ""
            ]
        , div [ class "controls row grow" ]
            [ viewReplayButton model ]
        , div [ class "status row grow" ]
            [ div [ class "radar-container" ] [ radar radarData ]
            , h3 [ class "points" ] [ text (String.fromInt model.points) ]
            ]
        ]


radarData : Radar.Points
radarData =
    [ { name = "C", value = 1 }
    , { name = "D", value = 3 }
    , { name = "E", value = 1 }
    , { name = "F", value = 2 }
    , { name = "G", value = 2 }
    , { name = "A", value = 2 }
    , { name = "B", value = 2 }
    ]


viewReplayButton : Model -> Html.Html Msg
viewReplayButton model =
    {--
    if model.playingSound then
        Images.view Images.soundWave

    else
      --}
    button
        [ class "button replay-button"
        , onClick (PlaySound model.currentKey)
        ]
        [ text "Replay Sound" ]


viewChoices : Model -> Html.Html Msg
viewChoices model =
    let
        choices =
            model.keysTesting

        widthPerChoice =
            widthFromChoiceCount (List.length choices)
    in
    div [ class "row choices" ]
        (List.map (viewChoice widthPerChoice model.answer model.currentKey) model.keysTesting)


viewChoice : String -> Maybe ( Note, Bool ) -> Note -> Note -> Html.Html Msg
viewChoice width prevAnswer correctNote note =
    let
        buttonClass =
            case prevAnswer of
                Nothing ->
                    "button choice"

                Just ( answeredNote, True ) ->
                    if answeredNote == note then
                        "button choice correct"

                    else
                        "button choice"

                Just ( answeredNote, False ) ->
                    if answeredNote == note then
                        "button choice incorrect"

                    else
                        "button choice"
    in
    button [ class buttonClass, style "flex-basis" width, onClick (Answer note) ] [ text note ]


widthFromChoiceCount : Int -> String
widthFromChoiceCount choiceCount =
    if choiceCount <= 4 then
        "50%"

    else
        "33%"


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
