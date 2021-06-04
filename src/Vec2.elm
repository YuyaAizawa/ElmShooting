module Vec2 exposing
  ( Vec2
  , nan
  , add
  , sub
  , mul
  , normal
  )

type alias Vec2 =
  { x : Float
  , y : Float
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