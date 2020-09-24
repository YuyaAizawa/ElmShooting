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
  , enemies : List Enemy
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

type alias Enemy =
  { tick : Int
  , move : Int -> EnemyProperty
  , shot : Player -> Int -> List Bullet
  , remained : Int -> Bool
  }

type alias EnemyProperty =
  { x : Float
  , y : Float
  , characteristics : Characteristics
  }

type Characteristics
  = ButterflyCharacteristics
      { a : Float
      , w : Float
      }

type alias Bullet =
  { shape : BulletShape
  , tick : Int
  , move : Int -> Position
  , remained : Int -> Bool
  }

type BulletShape
  = Middle

type alias Position =
  { x : Float
  , y : Float
  }
type alias Velocity =
  { x : Float
  , y : Float
  }



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
      let bcs = butterflyCurbedStrategy 0.003 in
      ( { model
        | scene = Flight
          { player = { x = 0.0, y = 10.0, r = 0.0 }
          , enemies =
            [
              { tick = 0
              , move = bcs
              , shot = shotAimPlayer bcs 2.0 200
              , remained = isOnStage << bcs
              }
            ]
          , bullets = []
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
          ( { model | scene =
              Flight
              { player = player |> playerMove model.keys
              , enemies = objectsUpdate enemies
              , bullets = enemiesShoot player enemies ++ objectsUpdate bullets
              }
            }
          , Cmd.none
          )

stage =
  { minX = -100.0
  , maxX =  100.0
  , minY =    0.0
  , maxY =  300.0
  }

movableMergin = 5.0
movableArea =
  { minX = stage.minX + movableMergin
  , maxX = stage.maxX - movableMergin
  , minY = stage.minY + movableMergin
  , maxY = stage.maxY - movableMergin
  }

playerMove : KeySet -> Player -> Player
playerMove keyset player =
  let
    d =
      if keyset |> Key.member Key.Shift
      then 1.5
      else 3.5

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
      keyset
        |> Key.foldl
          (\k r -> case k of
            Left -> r - 0.1
            Right -> r + 0.1
            _ -> r
          )
          player.r
        |> (\r ->
          let
            absr = abs player.r
          in
            if absr < 0.1
            then 0.0
            else player.r + player.r / absr * -0.1)
  in
  { player
    | x = nx
      |> max movableArea.minX
      |> min movableArea.maxX
    , y = ny
      |> max movableArea.minY
      |> min movableArea.maxY
    , r = nr
      |> max -0.6
      |> min  0.6
  }

type alias Object obj =
  { obj | tick : Int, remained : Int -> Bool }

objectsUpdate : List (Object obj) -> List (Object obj)
objectsUpdate objects =
  objects
    |> List.map (\obj -> { obj | tick = obj.tick + 1 })
    |> List.filter (\obj -> obj.remained obj.tick)

enemiesShoot : Player -> List Enemy -> List Bullet
enemiesShoot player enemies =
  enemies
    |> List.concatMap (\e -> e.shot player e.tick)



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
              ++ List.map (renderEnemy c) enemies
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

renderEnemy : Camera -> Enemy -> Svg msg
renderEnemy camera enemy =
  let
    property = enemy.move enemy.tick
  in
    case property.characteristics of
      ButterflyCharacteristics { a, w } ->
        renderButterfly camera property.x property.y a w

renderButterfly : Camera -> Float -> Float -> Float -> Float -> Svg msg
renderButterfly c x y a w =
  let
    w0 = sin w * 0.8
    w1 = sin (w + 0.4) * 0.8
    svgList =
      [ [ (  0.0,  0.0,  0.0 )
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
            ( pitch 0.2 >> yaw a >> translate ( x, 0.0, y ))
            >> renderPolyline c "#F30"
        )
  in
    Svg.g [] svgList

renderBullet : Camera -> Bullet -> Svg msg
renderBullet camera bullet =
  let
    p = bullet.move bullet.tick
  in
    case bullet.shape of
      Middle ->
        renderBall camera "#808" "#F0F" ( p.x, 0.0, p.y ) 3.0



-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.scene of
    _ ->
      Sub.batch
        [ Key.decoder KeyUpdate model.keys
        , onAnimationFrame (\_ -> Tick)
        ]



-- ENEMY --

butterflyCurbedStrategy : Float -> Int -> EnemyProperty
butterflyCurbedStrategy omega =
  \tick ->
    let
      theta = omega * toFloat tick
    in
      { x = 150.0 - 200.0 * cos theta
      , y = 300.0 - 200.0 * sin theta
      , characteristics =
        ButterflyCharacteristics
        { a = theta
        , w = toFloat tick * 0.07
        }
      }

shotAimPlayer : (Int -> EnemyProperty) -> Float -> Int -> Player -> Int -> List Bullet
shotAimPlayer pp speed interval =
  \player tick ->
    case modBy interval tick of
      0 ->
        let
          p = pp tick
          dx = player.x - p.x
          dy = player.y - p.y
          dd = sqrt (dx*dx + dy*dy)
        in
          [ cvlmMiddle { x = p.x, y = p.y } { x = dx / dd * speed, y = dy / dd * speed } ]

      _ ->
        []



-- BULLET --

cvlmMiddle : Position -> Velocity -> Bullet
cvlmMiddle pos vel =
  let
    predictor = cvlm pos vel
  in
    { shape = Middle
    , tick = 0
    , move = predictor
    , remained = isOnStage << predictor
    }

cvlm : Position -> Velocity -> Int -> Position
cvlm p0 v =
  \tick ->
    { x = p0.x + v.x * toFloat tick
    , y = p0.y + v.y * toFloat tick
    }



-- UTILITY --

margin = 10.0
isOnStage : { obj | x : Float, y : Float } -> Bool
isOnStage { x, y } =
  stage.minX < x + margin && x - margin < stage.maxX &&
  stage.minY < y + margin && y - margin < stage.maxY
