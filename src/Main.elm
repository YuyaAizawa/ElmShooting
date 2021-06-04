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

type Strategy a b =
  Strategy (a -> ( b, Strategy a b ))

type alias Player =
  Position
  { r : Float
  , cooldown : Int
  , bullets : List Bullet
  }

type alias Enemy =
  Object
  { shot : ShotStrategy
  , bullets : List Bullet
  }

type alias Bullet =
  Object
  {
  }

type alias Object misc =
  Position
  { misc
  | angle : Float
  , tick : Int
  , shape : Shape
  , move : MoveStrategy
  , survive : SurviveStrategy
  , isSurvive : Bool
  }

type alias Position misc =
  { misc
  | x : Float
  , y : Float
  }

type alias Vec2 =
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
                |> List.filter .isSurvive

            playerBullets_ =
              newPlayerBullets ++ existingPlayerBullets

            enemies_ =
              enemies
                |> List.map (updateEnemy player)
                |> List.filter .isSurvive

            newBullets =
              enemies_
                |> List.concatMap .bullets

            existingBullets =
              bullets
                |> List.map (updateBullet player)
                |> List.filter .isSurvive

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
    , bullets = playerShotNarrow <| getPosition this
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
    ( { x, y }, next ) =
      case this.move of
        Strategy s -> s (player |> getPosition)

    angle =
      atan2 (this.y - y) (this.x - x)
  in
    { this
    | x = x
    , y = y
    , angle = angle
    , move = next
    }

updateSurvive : Object a -> Object a
updateSurvive this =
  let
    ( isSurvive, next ) =
      case this.survive of
        Strategy s -> s (this |> getPosition)
  in
    { this
    | survive = next
    , isSurvive = isSurvive
    }

updateShot : Player -> Enemy -> Enemy
updateShot player this =
  let
    ( bullets, next ) =
      case this.shot of
        Strategy s -> s ( player |> getPosition, this |> getPosition )
  in
    { this
    | shot = next
    , bullets = bullets
    }

updateTick : Object a -> Object a
updateTick this =
  { this | tick = this.tick + 1 }




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
  case this.shape of
    Butterfly ->
      renderButterfly camera this.x this.y this.angle (toFloat this.tick * 0.07)

    BulletMiddle ->
      renderBall camera "#808" "#F0F" ( this.x, 0.0, this.y ) 3.0

    PlayerBullet ->
      renderPlayerBullet camera this.x this.y this.angle

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
    tail = ( x + cos a * l, 0.0, y + sin a * l)
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
  enemy
  { shape = Butterfly
  , move = circularMove { x = 150, y = 300 } 200.0 3.14 0.01
  , survive = surviveOnStage
  , shot = sampleShotStrategy
  }

enemy : EnemyProperty -> Enemy
enemy { shape, move, survive, shot } =
  { x = 0/0
  , y = 0/0
  , angle = 0/0
  , shape = shape
  , move = move
  , survive = survive
  , shot = shot
  , tick = 0
  , isSurvive = True
  , bullets = []
  }



-- BULLET --

playerBullet : Vec2 -> Vec2 -> Bullet
playerBullet p v =
  bullet
    { shape = PlayerBullet
    , move = cvlMove p v
    , survive = surviveOnStage
    }

bullet : ButtetProperty -> Bullet
bullet { shape, move, survive } =
  { x = 0/0
  , y = 0/0
  , angle = 0/0
  , shape = shape
  , move = move
  , survive = survive
  , tick = 0
  , isSurvive = True
  }



-- MOVE --

cvlMove : Vec2 -> Vec2 -> MoveStrategy
cvlMove p v =
  Strategy (\_ ->
    ( p, cvlMove (add p v) v ))

circularMove : Vec2 -> Float -> Float -> Float -> MoveStrategy
circularMove c r theta omega =
  Strategy (\_ ->
    ( { x = c.x + r * cos theta
      , y = c.y + r * sin theta
      }
    , circularMove c r (theta + omega) omega
    ))



-- SURVIVE --

surviveOnStage : SurviveStrategy
surviveOnStage =
  forever onStage ()

onStage : Vec2 -> Bool
onStage { x, y } =
  stage.minX < x + margin && x - margin < stage.maxX &&
  stage.minY < y + margin && y - margin < stage.maxY

margin = 10.0

surviveByTick : Int -> SurviveStrategy
surviveByTick remain =
  Strategy (\_ ->
    ( remain > 0
    , surviveByTick (remain - 1)
    )
  )



-- SHOT --

sampleShotStrategy : ShotStrategy
sampleShotStrategy =
  for 3 ( \a ->
    for 30 ( prev noShot )
    <| \_ -> prev (\pv -> [ shotAimAtPlayer BulletMiddle 1.0 pv ])
  a )
  <| forever noShot

shotAimAtPlayer : Shape -> Float -> ( Vec2, Vec2 ) -> Bullet
shotAimAtPlayer shape speed =
  \( p, e ) ->
    let
      v = mul speed (normal (sub p e))
    in
      bullet
      { shape = shape
      , move = cvlMove e v
      , survive = surviveOnStage
      }

noShot : ( Vec2, Vec2 ) -> List Bullet
noShot =
  always []

playerShotNarrow : Vec2 -> List Bullet
playerShotNarrow player =
  [ playerBullet (add player { x =  3.0, y = 0.0 }) { x = 0.0, y = 4.0 }
  , playerBullet (add player { x =  0.0, y = 2.0 }) { x = 0.0, y = 4.0 }
  , playerBullet (add player { x = -3.0, y = 0.0 }) { x = 0.0, y = 4.0 }
  ]



-- STRATEGY --

for : Int -> ((() -> Strategy a b) -> Strategy a b) -> (() -> Strategy a b) -> Strategy a b
for times target term =
  case times of
    0 -> term ()
    n -> target (\_ -> for (n - 1) target term)

forever : (a -> b) -> () -> Strategy a b
forever fun =
  \_ -> prev fun (forever fun)

prev : (a -> b) -> (() -> Strategy a b) -> Strategy a b
prev fun next =
  Strategy (\a -> ( fun a, next ()))



-- UTIL --

getPosition : Position a -> Vec2
getPosition this =
  { x = this.x
  , y = this.y
  }

nan : Vec2
nan =
  { x = 0/0
  , y = 0/0
  }

add : Vec2 -> Vec2 -> Vec2
add a b =
  { x = a.x + b.x
  , y = a.y + b.y
  }

sub : Vec2 -> Vec2 -> Vec2
sub a b =
  { x = a.x - b.x
  , y = a.y - b.y
  }

mul : Float -> Vec2 -> Vec2
mul c { x, y } =
  { x = c * x
  , y = c * y
  }

normal : Vec2 -> Vec2
normal { x, y } =
  let
    mag = sqrt ((x * x) + (y * y))
  in
    { x = x / mag
    , y = y / mag
    }