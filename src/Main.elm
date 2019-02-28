module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events
import Time
import Json.Decode as Json

import Debug exposing (toString)

import Key exposing (KeySet, Key(..))
import Chip

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

viewBoxWidth = 400
viewBoxHeight = 500



-- MODEL --

type alias Model =
  { scene : Scene
  , keys : KeySet
  }

type Scene
  = Title
  | Flight
    { player : Player }

type alias Player =
  { x : Int
  , y : Int
  }

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
      ( { model | scene = Flight { player = { x = 200, y = 450} } }, Cmd.none )
    TouchDown ->
      ( { model | scene = Title }, Cmd.none )
    Tick ->
      case model.scene of
        Title ->
          ( model , Cmd.none )
        Flight {player} ->
          (
            { model | scene =
              Flight { player = nextTick player model.keys }
            }
          , Cmd.none
          )

movableMergin = 10
movableArea =
  { xMin = movableMergin
  , xMax = viewBoxWidth - movableMergin
  , yMin = movableMergin
  , yMax = viewBoxHeight - movableMergin
  }

nextTick : Player -> KeySet -> Player
nextTick player keyset =
  let
    d = if keyset |> Key.member Key.Shift then 5 else 12

    nx = keyset
      |> Key.foldl
        (\k x -> case k of
          Left -> x-d
          Right -> x+d
          _ -> x
        )
        player.x

    ny = keyset
      |> Key.foldl
        (\k y -> case k of
          Up -> y-d
          Down -> y+d
          _ -> y
        )
        player.y
  in
  { player
    | x = nx
      |> max movableArea.xMin
      |> min movableArea.xMax
    , y = ny
      |> max movableArea.yMin
      |> min movableArea.yMax
  }



-- VIEW --

view : Model -> Html Msg
view model =
  case model.scene of
    Title ->
      Html.div []
        [ Svg.svg
          [ Attr.width (String.fromInt viewBoxWidth)
          , Attr.height (String.fromInt viewBoxHeight)
          , Attr.viewBox ("0 0 "++(String.fromInt viewBoxWidth)++" "++(String.fromInt viewBoxHeight))
          , Attr.style "background: #eee"
          ]
          [ Svg.text_
            [ Attr.x "200"
            , Attr.y "300"
            , Attr.fontSize "24"
            , Attr.textAnchor "middle"
            , Attr.class "fade-in-1s"
            , Svg.Events.onClick TakeOff
            ]
            [ Svg.text "Title" ]
          ]
        ]

    Flight {player}->
      Html.div []
        [ Svg.svg
          [ Attr.width "400"
          , Attr.height "500"
          , Attr.viewBox "0 0 400 500"
          , Attr.style "background: #333"
          ]
          [ Chip.player
              |> Chip.translate player.x player.y
          ]
        , Html.text (toString model.keys)
        ]



-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.scene of
    _ ->
      Sub.batch
        [ onKeyDown (keyDecoder True)
        , onKeyUp (keyDecoder False)
        , Time.every 30 (\_ -> Tick)
        ]

keyDecoder : Bool -> Json.Decoder Msg
keyDecoder pressed =
  let
    stringToMsg string =
      case string of
        "ArrowUp" -> KeyEvent Key.Up pressed
        "ArrowDown" -> KeyEvent Key.Down pressed
        "ArrowRight" -> KeyEvent Key.Right pressed
        "ArrowLeft" -> KeyEvent Key.Left pressed
        "Shift" -> KeyEvent Key.Shift pressed
        _ -> Nop
  in
    Json.map stringToMsg (Json.field "key" Json.string)