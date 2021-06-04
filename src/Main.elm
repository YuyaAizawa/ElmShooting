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
import Vec2 exposing (..)
import Renderer3D exposing (..)
import Strategy exposing (Strategy, strategy)


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
  , playerBullets : List Bullet
  , enemies : List Enemy
  , bullets : List Bullet
  }

init : () -> (Model, Cmd Msg)
init _ =
  (
    { scene = Title
    , keys = Key.empty
    }
  , Cmd.none
  )

type alias EnemyProperty =
  { shape : Shape
  , move : MoveStrategy
  , survive : SurviveStrategy
  , shot : ShotStrategy
  }

type alias ButtetProperty =
  { shape : Shape
  , move : MoveStrategy
  , survive : SurviveStrategy
  }

type Shape
  = Butterfly
  | BulletMiddle
  | PlayerBullet

getRadius : Shape -> Float
getRadius shape =
  case shape of
    Butterfly    -> 4.0
    BulletMiddle -> 3.0
    PlayerBullet -> 1.0

type alias MoveStrategy =
  Strategy Vec2 -- player
  Vec2

type alias SurviveStrategy =
  Strategy Vec2 -- object
  Bool

type alias ShotStrategy =
  Strategy ( Vec2, Vec2 ) -- ( player, enemy )
  (List Bullet)



type alias Player =
  { x : Float
  , y : Float
  , r : Float
  , cooldown : Int
  , bullets : List Bullet
  }

type alias Enemy =
  Object
  { shot : ShotStrategy
  }

type alias Bullet =
  Object
  {
  }

type alias Object misc =
  { misc
  | angle : Float
  , tick : Int
  , shape : Shape
  , move : MoveStrategy
  , survive : SurviveStrategy
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
      ( { model
        | scene = Flight
          { player =
            { x = 0.0
            , y = 10.0
            , r = 0.0
            , cooldown = 0
            , bullets = []
            }
          , playerBullets = []
          , enemies = [ sampleEnemy ]
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

        Flight { player, playerBullets, enemies, bullets } as flight ->
          let
            player_ =
              player
                |> playerMove model.keys
                |> playerShot model.keys

            newPlayerBullets =
              player_.bullets

            existingPlayerBullets =
              playerBullets
                |> List.map (updateBullet player)
                |> List.filter isSurvive

            playerBullets_ =
              newPlayerBullets ++ existingPlayerBullets

            enemies_ =
              enemies
                |> List.map (updateEnemy player)
                |> List.filter isSurvive

            newBullets =
              enemies_
                |> List.concatMap getBullets

            existingBullets =
              bullets
                |> List.map (updateBullet player)
                |> List.filter isSurvive

            bullets_ =
              newBullets ++ existingBullets

            scene =
              Flight
              { player = player_
              , playerBullets = playerBullets_
              , enemies = enemies_
              , bullets = bullets_
              }
          in
            ( { model | scene = scene }, Cmd.none )

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


updatePlayer : KeySet -> Player -> Player
updatePlayer keys this =
  this
    |> playerMove keys
    |> playerShot keys

playerMove : KeySet -> Player -> Player
playerMove keyset player =
  let
    d =
      if keyset |> Key.member Key.Shift
      then 0.7
      else 1.7

    nx =
      keyset
        |> Key.fold
          (\k x -> case k of
            Left -> x-d
            Right -> x+d
            _ -> x
          )
          player.x

    ny =
      keyset
        |> Key.fold
          (\k y -> case k of
            Up -> y+d
            Down -> y-d
            _ -> y
          )
          player.y

    nr =
      keyset
        |> Key.fold
          (\k r -> case k of
            Left -> r - 0.15
            Right -> r + 0.15
            _ -> r
          )
          (player.r + (abs player.r / player.r * -0.05))

  in
    { player
      | x = nx |> clamp movableArea.minX movableArea.maxX
      , y = ny |> clamp movableArea.minY movableArea.maxY
      , r = nr |> clamp -0.6 0.6
    }

playerShot : KeySet -> Player -> Player
playerShot keys this =
  if (keys |> Key.member Key.Space) && (this.cooldown == 0)
  then
    { this
    | cooldown = 7
    , bullets = playerShotNarrow <| playerPosition this
    }
  else
    { this
    | cooldown = max 0 (this.cooldown - 1)
    , bullets = []
    }

updateEnemy : Player -> Enemy -> Enemy
updateEnemy player this =
  this
    |> updatePosition player
    |> updateSurvive
    |> updateShot player
    |> updateTick

updateBullet : Player -> Bullet -> Bullet
updateBullet player this =
  this
    |> updatePosition player
    |> updateSurvive
    |> updateTick

updatePosition : Player -> Object a -> Object a
updatePosition player this =
  let
    move =
      this.move
        |> Strategy.update (playerPosition player)

    old =
      this.move
        |> Strategy.result

    new =
      move
        |> Strategy.result

    angle =
      atan2 (new.y - old.y) (new.x - old.x)
  in
    { this
    | move = move
    , angle = angle
    }

updateSurvive : Object a -> Object a
updateSurvive this =
  let
    survive =
      this.survive
        |> Strategy.update (getPosition this)
  in
    { this | survive = survive }

updateShot : Player -> Enemy -> Enemy
updateShot player this =
  let
    shot =
      this.shot
        |> Strategy.update ( playerPosition player, getPosition this )
  in
    { this | shot = shot }

updateTick : Object a -> Object a
updateTick this =
  { this | tick = this.tick + 1 }

getPosition : Object a -> Vec2
getPosition o =
  o.move
    |> Strategy.result

isSurvive : Object a -> Bool
isSurvive o =
  o.survive
    |> Strategy.result

getBullets : Enemy -> List Bullet
getBullets e =
  e.shot
    |> Strategy.result

playerPosition : Player -> Vec2
playerPosition { x, y } =
  { x = x
  , y = y
  }


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

    Flight { player, playerBullets, enemies, bullets } ->
      let
        c = camera ( player.x / 2.0 , -200.0, 180) -1.57 -2.6 350 ( -200.0, -300.0, 0 )
      in
        Html.div []
          [ Svg.svg
            [ SAttr.width (String.fromInt viewBoxWidth)
            , SAttr.height (String.fromInt viewBoxHeight)
            , SAttr.viewBox ("0 0 "++(String.fromInt viewBoxWidth)++" "++(String.fromInt viewBoxHeight))
            , SAttr.style "background: #333"
            ]
            ( [ renderStage c ]
              ++ List.map (renderObject c) playerBullets
              ++ [ renderPlayer c player ]
              ++ List.map (renderObject c) enemies
              ++ List.map (renderObject c) bullets
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

renderObject : Camera -> Object a -> Svg msg
renderObject camera this =
  let
    { x, y } = this |> getPosition
  in
    case this.shape of
      Butterfly ->
          renderButterfly camera x y this.angle (toFloat this.tick * 0.07)

      BulletMiddle ->
        renderBall camera "#808" "#F0F" ( x, 0.0, y ) 3.0

      PlayerBullet ->
        renderPlayerBullet camera x y this.angle

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
            ( pitch 0.2 >> yaw (a - (3.14/2) ) >> translate ( x, 0.0, y ))
            >> renderPolyline c "#F30"
        )
  in
    Svg.g [] svgList

renderPlayerBullet : Camera -> Float -> Float -> Float -> Svg msg
renderPlayerBullet c x y a =
  let
    l = 10.0
    head = ( x, 0.0, y )
    tail = ( x - cos a * l, 0.0, y - sin a * l)
  in
    Svg.g []
      [ renderBall c "none" "#CCC" head 1.0
      , renderPolyline c "#777" [ head, tail ]
      ]



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

sampleEnemy : Enemy
sampleEnemy =
  { shape = Butterfly
  , move = circularMove { x = 150, y = 300 } 200.0 3.14 0.005
  , survive = surviveOnStage
  , shot = sampleShotStrategy
  , tick = 0
  , angle = 0/0
  }



-- BULLET --

playerBullet : Vec2 -> Vec2 -> Bullet
playerBullet p v =
    { shape = PlayerBullet
    , move = cvlMove p v
    , survive = surviveOnStage
    , tick = 0
    , angle = 0/0
    }



-- MOVE --

cvlMove : Vec2 -> Vec2 -> MoveStrategy
cvlMove from v =
  let
    next _ p =
      add p v
  in
    strategy { init = from, update = next, view = identity }

circularMove : Vec2 -> Float -> Float -> Float -> MoveStrategy
circularMove center radius thetaZero omega =
  let
    next _ theta
      = theta + omega

    output theta =
      { x = center.x + radius * cos theta
      , y = center.y + radius * sin theta
      }
  in
    strategy { init = thetaZero, update = next, view = output }



-- SURVIVE --

surviveOnStage : SurviveStrategy
surviveOnStage =
  strategy { init = { x = 0.0, y = 0.0 }, update = \p _ -> p, view = onStage }

onStage : Vec2 -> Bool
onStage { x, y } =
  stage.minX < x + margin && x - margin < stage.maxX &&
  stage.minY < y + margin && y - margin < stage.maxY

margin = 10.0

surviveByTick : Int -> SurviveStrategy
surviveByTick remaining =
  let
    decrement _ r = r - 1
    isRemaining r = r > 0
  in
    strategy { init = remaining, update = decrement, view = isRemaining }



-- SHOT --

sampleShotStrategy : ShotStrategy
sampleShotStrategy =
  let
    update_ pv ( n, m, _ ) =
      if n == -1
      then ( -1,   -1, pv )
      else if m == 0
      then ( n-1,  80, pv )
      else (   n, m-1, pv )

    view_ ( n, m, pv ) =
      if m == 0
      then [ shotAimAtPlayer BulletMiddle 1.0 pv ]
      else []
  in
    strategy { init = ( 2, 80, ( nan, nan ) ), update = update_, view = view_ }

shotAimAtPlayer : Shape -> Float -> ( Vec2, Vec2 ) -> Bullet
shotAimAtPlayer shape speed =
  \( p, e ) ->
    let
      v = mul speed (normal (sub p e))
    in
      { shape = shape
      , move = cvlMove e v
      , survive = surviveOnStage
      , tick = 0
      , angle = 0/0
      }

playerShotNarrow : Vec2 -> List Bullet
playerShotNarrow player =
  [ playerBullet (add player { x =  3.0, y = 0.0 }) { x = 0.0, y = 4.0 }
  , playerBullet (add player { x =  0.0, y = 2.0 }) { x = 0.0, y = 4.0 }
  , playerBullet (add player { x = -3.0, y = 0.0 }) { x = 0.0, y = 4.0 }
  ]
