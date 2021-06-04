module Strategy exposing
  ( Strategy
  , strategy
  , update
  , result
  )

type Strategy i o =
  Strategy
  { updater : i -> Strategy i o
  , output : o
  }

strategy :
  { init : s
  , update : i -> s -> s
  , view : s -> o
  } -> Strategy i o
strategy r =
  let
    updater i = -- remove type variable s
      strategy { r | init = r.update i r.init }
  in
    Strategy
    { updater = updater
    , output = r.view r.init
    }

update : i -> Strategy i o -> Strategy i o
update input (Strategy { updater }) =
  updater input

result : Strategy i o -> o
result (Strategy { output } ) =
  output
