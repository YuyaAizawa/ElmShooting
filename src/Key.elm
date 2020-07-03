module Key exposing
  ( Key(..)
  , decoder
  --
  , KeySet
  , empty
  , member
  , insert
  , remove
  , foldl
  , foldr
  , toList
  , fromList
  )

import Browser.Events exposing (onKeyUp, onKeyDown)
import Json.Decode as Json
import Set exposing (Set)

type Key
  = Up
  | Down
  | Left
  | Right
  | Space
  | Shift

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
  = KeySet (Set Int)

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
    Space -> 5
    Shift -> 6

fromInt : Int -> Key
fromInt int =
  case int of
    1 -> Up
    2 -> Down
    3 -> Left
    4 -> Right
    5 -> Space
    _ -> Shift