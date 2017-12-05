module Day1.Solution exposing (part1, part2)

import Day1.Data exposing (data)
import Array

part1 = solve data
part2 = 0

parseInt : String -> Int
parseInt char =
  Result.withDefault 0 (String.toInt char)

toIntList : String -> List Int
toIntList str =
  List.map
    (parseInt << String.fromChar)
    (String.toList str)

compareSame : Int -> Int -> Int
compareSame x y =
  if x == y then x else 0

solve : String -> Int
solve str =
  let
    array = Array.fromList (toIntList str)
    lastResult = compareSame
      (Maybe.withDefault 0 (Array.get 0 array))
      (Maybe.withDefault 0 (Array.get ((Array.length array) - 1) array))
  in lastResult + (Array.foldr
    (\current tuple ->
      (current, (Tuple.second tuple) + compareSame (Tuple.first tuple) current))
    (0, 0)
    array
    |> Tuple.second)
