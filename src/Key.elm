module Key exposing
  ( KeySet
  , Key(..)
  , empty
  , member
  , insert
  , remove
  )

import Set exposing (Set)

type KeySet
  = KeySet (Set Int)

type Key
  = Up
  | Down
  | Left
  | Right


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


toInt : Key -> Int
toInt key =
  case key of
    Up -> 1
    Down -> 2
    Left -> 3
    Right -> 4
