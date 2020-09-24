module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, br)
import Html.Attributes as HAttr
import Html.Events
import Json.Decode as Json
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events

import Debug exposing (toString)

import Key exposing (KeySet, Key(..))
import Renderer3D exposing (..)

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
  | Flight FlightModel

type alias FlightModel =
  { player : Player
  , enemies : List Butterfly
  , bullets : List Bullet
  }

type alias Player =
  { x : Float
  , y : Float
  , r : Float
  }

init : () -> (Model, Cmd Msg)
init _ =
  (
    { scene = Title
    , keys = Key.empty
    }
  , Cmd.none
  )

type alias Butterfly =
  { x : Float
  , y : Float
  , t : Int
  }

type Bullet
  = CvlmMiddle Position Velocity

type alias Position = ( Float, Float )
type alias Velocity = ( Float, Float )



-- UPDATE --

type Msg
  = Nop
  | TakeOff
  | TouchDown
  | Tick
  | KeyUpdate KeySet

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Nop ->
      ( model, Cmd.none )
    KeyUpdate keys ->
      ( { model | keys = keys }, Cmd.none )
    TakeOff ->
      ( { model
        | scene = Flight
          { player = { x = 0.0, y = 10.0, r = 0.0 }
          , enemies = [ { x = 0.0, y = 150.0, t = 0} ]
          , bullets = [ CvlmMiddle ( 0.0, 200.0 ) ( 0.1, -1.0 ) ]
          }
        }
      , Cmd.none
      )
    TouchDown ->
      ( { model | scene = Title }, Cmd.none )
    Tick ->
      case model.scene of
        Title ->
          ( model , Cmd.none )
        Flight { player, enemies, bullets } ->
          (
            { model | scene =
              Flight
              { player = nextTick player model.keys
              , enemies = enemiesUpdate enemies
              , bullets = bulletsUpdate bullets
              }
            }
          , Cmd.none
          )


movableMergin = 5
movableArea =
  { xMin = -100 + movableMergin
  , xMax =  100 - movableMergin
  , yMin =    0 + movableMergin
  , yMax =  300 - movableMergin
  }

nextTick : Player -> KeySet -> Player
nextTick player keyset =
  let
    d = if keyset |> Key.member Key.Shift then 1.5 else 3.5

    nx =
      keyset
        |> Key.foldl
          (\k x -> case k of
            Left -> x-d
            Right -> x+d
            _ -> x
          )
          player.x

    ny =
      keyset
        |> Key.foldl
          (\k y -> case k of
            Up -> y+d
            Down -> y-d
            _ -> y
          )
          player.y

    nr =
      if Key.member Key.Left keyset
      then player.r - 0.1
      else if Key.member Key.Right keyset
      then player.r + 0.1
      else if player.r > 0
      then
        if player.r > 0.1
        then player.r - 0.1
        else 0.0
      else if player.r < 0
      then
        if player.r < -0.1
        then player.r + 0.1
        else 0.0
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
      |> max -0.6
      |> min  0.6
  }
enemiesUpdate: List Butterfly -> List Butterfly
enemiesUpdate enemies =
  enemies
    |> List.map (\e -> { e | t = e.t + 1 })

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
          [ SAttr.width (String.fromInt viewBoxWidth)
          , SAttr.height (String.fromInt viewBoxHeight)
          , SAttr.viewBox ("0 0 "++(String.fromInt viewBoxWidth)++" "++(String.fromInt viewBoxHeight))
          , SAttr.style "background: #eee"
          ]
          [ Svg.text_
            [ SAttr.x "200"
            , SAttr.y "300"
            , SAttr.fontSize "24"
            , SAttr.textAnchor "middle"
            , SAttr.class "fade-in-1s"
            , Svg.Events.onClick TakeOff
            ]
            [ Svg.text "Title" ]
          ]
        ]

    Flight flightModel ->
      let
        player = flightModel.player
        enemies = flightModel.enemies
        bullets = flightModel.bullets
        c = camera ( player.x / 2.0 , -200.0, 180) -1.57 -2.6 350 ( -200.0, -300.0, 0 )
      in
        Html.div []
          [ Svg.svg
            [ SAttr.width (String.fromInt viewBoxWidth)
            , SAttr.height (String.fromInt viewBoxHeight)
            , SAttr.viewBox ("0 0 "++(String.fromInt viewBoxWidth)++" "++(String.fromInt viewBoxHeight))
            , SAttr.style "background: #333"
            ]
            ( [ renderStage c
              , renderPlayer c player
              ]
              ++ List.map (renderButterfly c) enemies
              ++ List.map (renderBullet c) bullets
            )
          ]

renderStage : Camera -> Svg msg
renderStage c =
  Svg.g []
  [ renderPolyline c "#666"
    [ (  -33.0, 0.0,   0.0 )
    , (  -33.0, 0.0, 300.0 )
    , (   33.0, 0.0, 300.0 )
    , (   33.0, 0.0,   0.0 )
    ]
  , renderPolyline c "#666"
    [ (  -100.0, 0.0, 100.0 )
    , (  -100.0, 0.0, 200.0 )
    , (   100.0, 0.0, 200.0 )
    , (   100.0, 0.0, 100.0 )
    , (  -100.0, 0.0, 100.0 )
    ]
  , renderPolyline c "#EEE"
    [ ( -100.0, 0.0,   0.0 )
    , ( -100.0, 0.0, 300.0 )
    , (  100.0, 0.0, 300.0 )
    , (  100.0, 0.0,   0.0 )
    ]
  ]

renderPlayer : Camera -> Player -> Svg msg
renderPlayer c { x, y, r } =
  Svg.g []
  [ renderBall c "#800" "#F00" ( x, 0.0, y ) 4.0
  , [ (   0.0,  5.0,  0.0 )
    , (   0.0,  0.0, 10.0 )
    , ( -10.0, -1.0, -4.0 )
    , (   0.0,  5.0,  0.0 )
    ] |> List.map (roll r)
      |> List.map (translate ( x, 0.0, y ))
      |> renderPolyline c "#AAA"

  , [ (   0.0,  5.0,  0.0 )
    , (   0.0,  0.0, 10.0 )
    , (  10.0, -1.0, -4.0 )
    , (   0.0,  5.0,  0.0 )
    ] |> List.map (roll r)
      |> List.map (translate ( x, 0.0, y ))
      |> renderPolyline c "#888"
  ]

renderButterfly : Camera -> Butterfly -> Svg msg
renderButterfly c { x, y, t } =
  let
    w0 = sin (toFloat t * 0.07) * 0.8
    w1 = sin (toFloat t * 0.07 + 0.4) * 0.8
  in
    Svg.g []
    ( [ [ (  0.0,  0.0,  0.0 )
        , ( 10.0, 10.0,  0.0 )
        , (  7.0,  0.0,  0.0 )
        , (  0.0,  0.0,  0.0 )
        ] |> List.map (yaw w1)
      , [ (  0.0,  0.0,  0.0 )
        , (  7.0, -3.0,  0.0 )
        , (  3.0,-10.0,  0.0 )
        , (  0.0,  0.0,  0.0 )
        ] |> List.map (yaw w0)
      , [ (  0.0,  0.0,  0.0 )
        , (-10.0, 10.0,  0.0 )
        , ( -7.0,  0.0,  0.0 )
        , (  0.0,  0.0,  0.0 )
        ] |> List.map (yaw -w1)
      , [ (  0.0,  0.0,  0.0 )
        , ( -7.0, -3.0,  0.0 )
        , ( -3.0,-10.0,  0.0 )
        , (  0.0,  0.0,  0.0 )
        ] |> List.map (yaw -w0)
      ] |> List.map
        ( List.map
            ( pitch 0.2 >> translate ( x, 0.0, y ))
            >> renderPolyline c "#F30"
        )
    )

renderBullet : Camera -> Bullet -> Svg msg
renderBullet c b =
  case b of
    CvlmMiddle ( x, y ) _ ->
      renderBall c "#808" "#F0F" ( x, 0.0, y ) 3.0



-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.scene of
    _ ->
      Sub.batch
        [ Key.decoder KeyUpdate model.keys
        , onAnimationFrame (\_ -> Tick)
        ]
