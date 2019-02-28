module Key exposing
  ( KeySet
  , Key(..)
  , empty
  , member
  , insert
  , remove
  , foldl
  , foldr
  , toList
  , fromList
  )

import Set exposing (Set)

type KeySet
  = KeySet (Set Int)

type Key
  = Up
  | Down
  | Left
  | Right
  | Shift


empty : KeySet
empty =
  KeySet (Set.empty)

member : Key -> KeySet -> Bool
member key keyset =
  case keyset of
    KeySet contents ->
      contents
        |> Set.member (key |> toInt)

insert : Key -> KeySet -> KeySet
insert key keyset =
  case keyset of
    KeySet contents ->
      contents
        |> Set.insert (key |> toInt)
        |> KeySet

remove : Key -> KeySet -> KeySet
remove key keyset =
  case keyset of
    KeySet contents ->
      contents
        |> Set.remove (key |> toInt)
        |> KeySet

foldl : (Key -> a -> a) -> a -> KeySet -> a
foldl acc init keyset =
  case keyset of
    KeySet contents ->
      contents |> Set.foldl (acc << fromInt) init

foldr : (Key -> a -> a) -> a -> KeySet -> a
foldr acc init keyset =
  case keyset of
    KeySet contents ->
      contents |> Set.foldr (acc << fromInt) init

toList : KeySet -> List Key
toList =
  foldl (::) []

fromList : List Key -> KeySet
fromList =
  List.foldl insert empty


toInt : Key -> Int
toInt key =
  case key of
    Up -> 1
    Down -> 2
    Left -> 3
    Right -> 4
    Shift -> 5

fromInt : Int -> Key
fromInt int =
  case int of
    1 -> Up
    2 -> Down
    3 -> Left
    4 -> Right
    _ -> Shift