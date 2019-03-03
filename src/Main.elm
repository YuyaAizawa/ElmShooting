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
    { player : Player
    , bullets : List Bullet
    }

type alias Player =
  { x : Int
  , y : Int
  , r : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  (
    { scene = Title
    , keys = Key.empty
    }
  , Cmd.none
  )

type Bullet
  = CvlmMiddle Position Velocity

type alias Position = (Float, Float)
type alias Velocity = (Float, Float)



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
      ( { model | scene = Flight { player = { x = 200, y = 450, r = 0}, bullets = [CvlmMiddle (200.0, 0.0) (0.0, 1.0)] } }, Cmd.none )
    TouchDown ->
      ( { model | scene = Title }, Cmd.none )
    Tick ->
      case model.scene of
        Title ->
          ( model , Cmd.none )
        Flight {player, bullets} ->
          (
            { model | scene =
              Flight
              { player = nextTick player model.keys
              , bullets = bulletsUpdate bullets
              }
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

    nr = if Key.member Key.Left keyset then player.r + 1
      else if Key.member Key.Right keyset then player.r - 1
      else if player.r > 0 then player.r - 1
      else if player.r < 0 then player.r + 1
      else player.r
  in
  { player
    | x = nx
      |> max movableArea.xMin
      |> min movableArea.xMax
    , y = ny
      |> max movableArea.yMin
      |> min movableArea.yMax
    , r = nr
      |> max -6
      |> min  6
  }

bulletsUpdate: List Bullet -> List Bullet
bulletsUpdate bullets =
  bullets
    |> List.map bulletMove
    |> List.filter bulletRemained

bulletMove: Bullet -> Bullet
bulletMove bullet =
  case bullet of
    CvlmMiddle (x, y) (vx, vy) ->
      CvlmMiddle (x+vx, y+vy) (vx, vy)

cvlmMargin = 10
bulletRemained: Bullet -> Bool
bulletRemained bullet =
  case bullet of
    CvlmMiddle (x, y) _ ->
     (-cvlmMargin < x) && (x < viewBoxWidth + cvlmMargin) &&
     (-cvlmMargin < y) && (y < viewBoxHeight + cvlmMargin)



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

    Flight {player, bullets} ->
      Html.div []
        [ Svg.svg
          [ Attr.width (String.fromInt viewBoxWidth)
          , Attr.height (String.fromInt viewBoxHeight)
          , Attr.viewBox ("0 0 "++(String.fromInt viewBoxWidth)++" "++(String.fromInt viewBoxHeight))
          , Attr.style "background: #333"
          ]
          ((playerView player) :: (bullets |> List.map bulletView))
        , Html.text (toString model.keys)
        ]

playerView: Player -> Svg msg
playerView player =
  Chip.player player.x player.y player.r

bulletView: Bullet -> Svg msg
bulletView bullet =
  case bullet of
    CvlmMiddle (x, y) _ ->
      Chip.bulletMiddle (round x) (round y)



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
