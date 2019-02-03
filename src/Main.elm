port module Main exposing (Model, Msg(..), Note, init, initialModel, main, playSound, subscriptions, update, view, viewLink)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process
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
    , correctAnswer : Bool
    }


type alias Note =
    String


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
    , correctAnswer = False
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
    | Answer Note
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
            ( model, playSound note )

        Answer note ->
            if note == model.currentKey then
                ( { model | points = model.points + 100, correctAnswer = True }
                , Cmd.batch
                    [ playSound note
                    , after 1000 NextLevel
                    ]
                )

            else
                ( model, playSound note )

        NextLevel ->
            let
                newModel =
                    { model | correctAnswer = False }
                        |> nextKey
            in
            ( newModel, after 200 <| PlaySound newModel.currentKey )


nextKey : Model -> Model
nextKey model =
    let
        ( newKey, keysRemaining ) =
            case List.head model.keysToTest of
                Just key ->
                    ( key, List.tail model.keysToTest |> Maybe.withDefault model.keysToTest )

                Nothing ->
                    -- this should never happen
                    ( "C", model.keysToTest )
    in
    { model
        | homescreen = False
        , currentKey = newKey
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
    div [ class "column center" ]
        [ h1 [] [ text "Pitch Perfect" ]
        , button [ class "start-button button", onClick StartGame ] [ h2 [] [ text "Start" ] ]
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
        , div [ class "controls row grow" ]
            [ button [ class "button replay-button", onClick (PlaySound model.currentKey) ] [ text "Replay Sound" ] ]
        , div [ class "status row grow" ]
            [ h3 [ class "points" ] [ text (String.fromInt model.points) ] ]
        ]


viewChoices : Model -> Html.Html Msg
viewChoices model =
    let
        choices =
            model.keysTesting

        widthPerChoice =
            widthFromChoiceCount (List.length choices)
    in
    div [ class "row choices" ]
        (List.map (viewChoice widthPerChoice model.correctAnswer model.currentKey) model.keysTesting)


viewChoice : String -> Bool -> Note -> Note -> Html.Html Msg
viewChoice width correct correctNote note =
    let
        buttonClass =
            if correct && correctNote == note then
                "button choice correct"

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
