module Chip exposing
  ( player
  , translate
  )

import Svg exposing (..)
import Svg.Attributes exposing (..)

player: List (Svg msg)
player =
  [ Circle  0  0  9 darkRed red
  , Circle -3 -3  5 none lightRed
  , Polygon [(0, -21), (0, 4), ( 19, 14)] gray none
  , Polygon [(0, -21), (0, 4), (-19, 14)] lightGray none
  ]
    |> List.map toSvg



type Figure
  = Polygon (List (Int, Int)) Color Color
  | Polyline (List (Int, Int)) Color
  | Circle Int Int Int Color Color

toSvg: Figure -> Svg msg
toSvg obj =
  case obj of
    Polygon pl s_ f_ ->
      polygon
        [points (
          pl
            |> List.map (\(x_, y_) -> String.fromInt x_ ++ "," ++ String.fromInt y_)
            |> List.intersperse " "
            |> String.concat)
        , stroke s_
        , fill f_
        ][]

    Polyline pl c_ ->
      polyline
        [points (
          pl
            |> List.map (\(x_, y_) -> String.fromInt x_ ++ "," ++ String.fromInt y_)
            |> List.intersperse " "
            |> String.concat)
        , stroke c_
        , fill "none"
        ][]

    Circle x_ y_ r_ s_ f_ ->
      circle
        [ cx <| String.fromInt <| x_
        , cy <| String.fromInt <| y_
        , r <| String.fromInt <| r_
        , stroke s_
        , fill f_
        ][]

translate: Int -> Int -> List (Svg msg) -> Svg msg
translate x y contents =
  Svg.g
    [ transform <| "translate(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")" ]
    contents


type alias Color = String

none = "none"


red = "#FF0000"
lightRed = "#FF8888"
darkRed = "#880000"
yellow = "#FFFF00"
lightYellow = "#FFFF88"
green = "#00FF00"
blue = "#0000FF"
lightBlue = "#8888FF"
darkBlue = "#000088"
magenta = "#FF00FF"

white = "#FFFFFF"
lightGray = "#AAAAAA"
gray = "#888888"
black = "#000000"