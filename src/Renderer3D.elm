module Renderer3D exposing
  ( Point3D
  , Camera
  , camera
  , renderPolyline
  , renderBall
  )

import Svg exposing (Svg)
import Svg.Attributes as Attr

type alias Point3D = ( Float, Float, Float )

type Camera = Camera Matrix4D

camera : Point3D -> Float -> Float -> Float -> Point3D -> Camera
camera position theta phi depth viewOffset =
  Camera
    <| matmul4D (translation position)
    <| matmul4D (rotation theta phi)
    <| matmul4D (zPerspectiveTransformation depth)
    <| (translation viewOffset)

rotation : Float -> Float -> Matrix4D
rotation theta phi =
  Matrix4D
  -(sin theta) -(cos theta * sin phi) (cos theta * cos phi) 0.0
   (cos theta) -(sin theta * sin phi) (sin theta * cos phi) 0.0
          0.0               (cos phi)             (sin phi) 0.0
          0.0                    0.0                   0.0  1.0

translation : Point3D -> Matrix4D
translation (px, py, pz) =
  Matrix4D
  1.0 0.0 0.0 0.0
  0.0 1.0 0.0 0.0
  0.0 0.0 1.0 0.0
  -px -py -pz 1.0

zPerspectiveTransformation : Float -> Matrix4D
zPerspectiveTransformation r =
  Matrix4D
  1.0 0.0 0.0 0.0
  0.0 1.0 0.0 0.0
  0.0 0.0 1.0 (-1/r)
  0.0 0.0 0.0 1.0

wNormalization : Vector4D -> Vector4D
wNormalization (Vector4D x y z w) =
  Vector4D (x/w) (y/w) (z/w) 1.0

renderPolyline : Camera -> String -> List Point3D -> Svg msg
renderPolyline (Camera cameraMatrix) color vertexes =
  let
    points =
      vertexes
        |> List.map (
          \(x, y, z) ->
            case
              mvmul4D (Vector4D x y z 1.0) cameraMatrix
                |> wNormalization
            of
              Vector4D x_ y_ _ _ ->
                String.fromFloat x_ ++ "," ++ String.fromFloat y_)
        |> String.join " "
  in
    Svg.polyline
    [ Attr.points points
    , Attr.stroke color
    , Attr.fill "none"
    ][]

renderBall : Camera -> String -> String -> Point3D -> Float -> Svg msg
renderBall (Camera cameraMatrix) stroke fill ( x, y, z ) r =
  let
    ((Vector4D x_ y_ z_ w_) as v) =
      mvmul4D (Vector4D x y z 1.0) cameraMatrix

    (Vector4D x__ y__ _ _) =
      wNormalization v

    r__ = r/w_

  in
    Svg.circle
    [ Attr.cx <| String.fromFloat x__
    , Attr.cy <| String.fromFloat y__
    , Attr.r  <| String.fromFloat r__
    , Attr.stroke stroke
    , Attr.fill fill
    ][]

type Vector4D = Vector4D
  Float Float Float Float

type Matrix4D = Matrix4D
  Float Float Float Float
  Float Float Float Float
  Float Float Float Float
  Float Float Float Float

unitMat4D : Matrix4D
unitMat4D =
  (Matrix4D
    1.0 0.0 0.0 0.0
    0.0 1.0 0.0 0.0
    0.0 0.0 1.0 0.0
    0.0 0.0 0.0 1.0
  )

matmul4D : Matrix4D -> Matrix4D -> Matrix4D
matmul4D
  (Matrix4D
    a11 a12 a13 a14
    a21 a22 a23 a24
    a31 a32 a33 a34
    a41 a42 a43 a44
  )
  (Matrix4D
    b11 b12 b13 b14
    b21 b22 b23 b24
    b31 b32 b33 b34
    b41 b42 b43 b44
  )
  =
    (Matrix4D
      (a11*b11 + a12*b21 + a13*b31 + a14*b41)
      (a11*b12 + a12*b22 + a13*b32 + a14*b42)
      (a11*b13 + a12*b23 + a13*b33 + a14*b43)
      (a11*b14 + a12*b24 + a13*b34 + a14*b44)
      (a21*b11 + a22*b21 + a23*b31 + a24*b41)
      (a21*b12 + a22*b22 + a23*b32 + a24*b42)
      (a21*b13 + a22*b23 + a23*b33 + a24*b43)
      (a21*b14 + a22*b24 + a23*b34 + a24*b44)
      (a31*b11 + a32*b21 + a33*b31 + a34*b41)
      (a31*b12 + a32*b22 + a33*b32 + a34*b42)
      (a31*b13 + a32*b23 + a33*b33 + a34*b43)
      (a31*b14 + a32*b24 + a33*b34 + a34*b44)
      (a41*b11 + a42*b21 + a43*b31 + a44*b41)
      (a41*b12 + a42*b22 + a43*b32 + a44*b42)
      (a41*b13 + a42*b23 + a43*b33 + a44*b43)
      (a41*b14 + a42*b24 + a43*b34 + a44*b44)
    )

mvmul4D : Vector4D -> Matrix4D -> Vector4D
mvmul4D
  (Vector4D
    v1 v2 v3 v4
  )
  (Matrix4D
    a11 a12 a13 a14
    a21 a22 a23 a24
    a31 a32 a33 a34
    a41 a42 a43 a44
  )
  =
    (Vector4D
      (a11*v1 + a21*v2 + a31*v3 + a41*v4)
      (a12*v1 + a22*v2 + a32*v3 + a42*v4)
      (a13*v1 + a23*v2 + a33*v3 + a43*v4)
      (a14*v1 + a24*v2 + a34*v3 + a44*v4)
    )