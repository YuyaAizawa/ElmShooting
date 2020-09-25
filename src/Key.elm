module Key exposing
  ( Key(..)
  , decoder
  --
  , KeySet
  , empty
  , member
  , insert
  , remove
  , fold
  , toList
  , fromList
  )

import Browser.Events exposing (onKeyUp, onKeyDown)
import Json.Decode as Json
import Bitwise exposing (..)



-- KEY

type Key
  = Up
  | Down
  | Left
  | Right
  | Space
  | Shift

all =
  [ Up
  , Down
  , Left
  , Right
  , Space
  , Shift
  ]

mask : Key -> Int
mask key =
  case key of
    Up    ->  1
    Down  ->  2
    Left  ->  4
    Right ->  8
    Space -> 16
    Shift -> 32

decoder : (KeySet -> msg) -> KeySet -> Sub msg
decoder msgMapper previous =
  Sub.batch
  [ onKeyDown <| keyUpdater insert previous <| msgMapper
  , onKeyUp   <| keyUpdater remove previous <| msgMapper
  ]

keyUpdater: (Key -> KeySet -> KeySet) -> KeySet -> (KeySet -> msg) -> Json.Decoder msg
keyUpdater op previous msgMapper =
  Json.field "key" Json.string
    |> Json.map
      (fromName
        >> Maybe.map (\key -> op key previous)
        >> Maybe.withDefault previous
        >> msgMapper
      )

fromName : String -> Maybe Key
fromName key =
  case key of
    "ArrowUp"    -> Just Up
    "ArrowDown"  -> Just Down
    "ArrowLeft"  -> Just Left
    "ArrowRight" -> Just Right
    " "          -> Just Space
    "Shift"      -> Just Shift
    _            -> Nothing



-- KEY SET

type KeySet
  = KeySet Int

empty : KeySet
empty =
  KeySet 0

member : Key -> KeySet -> Bool
member key (KeySet n) =
  mask key
    |> and n
    |> (/=) 0

insert : Key -> KeySet -> KeySet
insert key (KeySet n) =
  mask key
    |> or n
    |> KeySet

remove : Key -> KeySet -> KeySet
remove key (KeySet n) =
  mask key
    |> complement
    |> and n
    |> KeySet

fold : (Key -> a -> a) -> a -> KeySet -> a
fold acc init keyset =
  let
    acc_ = \k a ->
      if member k keyset
      then acc k a
      else a
  in
    List.foldl acc_ init all

toList : KeySet -> List Key
toList =
  fold (::) []

fromList : List Key -> KeySet
fromList =
  List.foldl insert empty
