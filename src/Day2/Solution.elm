module Day2.Solution exposing (part1, part2)

import Day2.Data exposing (data)

part1 = solve1 data
part2 = 0

convert : String -> List (List Int)
convert str = str
  |> String.trim
  |> String.split "\n"
  |> List.map (String.split " ")
  |> List.map (List.map String.toInt)
  |> List.map (List.map (Result.withDefault 0))

solve1 : String -> Int
solve1 str =
  let
    list = convert str
    minList = List.map (List.minimum >> Maybe.withDefault 0) list
    maxList = List.map (List.maximum >> Maybe.withDefault 0) list
    diffs = List.map2 (\x y -> x - y) maxList minList
  in
    List.sum diffs

solve2 : String -> Int
solve2 str =
  0
