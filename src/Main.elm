module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Time
import Json.Decode as Json

import Key exposing (KeySet, Key(..))

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL --

type alias Model =
  { scene : Scene
  , keys : KeySet
  }

type Scene
  = Title
  | Flight

init : () -> (Model, Cmd Msg)
init _ =
  (
    { scene = Title
    , keys = Key.empty
    }
  , Cmd.none
  )



-- UPDATE --

type Msg
  = Nop
  | TakeOff
  | TouchDown
  | Tick
  | KeyEvent Key Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Nop ->
      ( model, Cmd.none )
    KeyEvent key True ->
      ( { model | keys = model.keys |> Key.insert key }, Cmd.none )
    KeyEvent key False ->
      ( { model | keys = model.keys |> Key.remove key }, Cmd.none )
    TakeOff ->
      ( { model | scene = Flight }, Cmd.none )
    TouchDown ->
      ( { model | scene = Title }, Cmd.none )
    Tick ->
      ( model, Cmd.none )



view : Model -> Html Msg
view model =
  Svg.svg
    [ Attr.width "400"
    , Attr.height "500"
    , Attr.viewBox "0 0 300 400"
    , Attr.style "background: #eee"
    ][]



-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.scene of
    Flight ->
      Sub.batch
        [ onKeyDown (keyDecoder True)
        , onKeyUp (keyDecoder False)
        , Time.every 30 (\_ -> Tick)
        ]
    _ -> Sub.none

keyDecoder : Bool -> Json.Decoder Msg
keyDecoder pressed =
  let
    stringToMsg string =
      case string of
        "ArrowUp" -> KeyEvent Key.Up pressed
        "ArrowDown" -> KeyEvent Key.Down pressed
        "ArrowRight" -> KeyEvent Key.Right pressed
        "ArrowLeft" -> KeyEvent Key.Left pressed
        _ -> Nop
  in
    Json.map stringToMsg (Json.field "key" Json.string)