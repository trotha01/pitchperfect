port module Main exposing (..)


import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
  { key : Nav.Key
  , url : Url.Url
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url, Cmd.none )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | PlaySound Sound

type alias Sound =
       { instrument : String
       , note : String
       , octave : Int
       , duration : Int
       }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
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
    PlaySound sound ->
            ( model, playSound sound)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- PORTS
port playSound : Sound -> Cmd msg

-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body =
      [ img [src "treble_clef.svg", style "height" "5em", style "float" "left" ] []
     ,  hr [ style "margin" "1em"] []
     ,  hr [ style "margin" "1em"] []
     ,  hr [ style "margin" "1em"] []
     ,  hr [ style "margin" "1em"] []
     ,  hr [ style "margin" "1em"] []
              , button [ onClick (PlaySound {
              instrument = "piano",
              note = "C",
              octave = 4,
              duration = 1
  }) ] [ text "Play Sound" ]
      ]
  }


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
