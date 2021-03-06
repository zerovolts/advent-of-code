module Day1.Solution exposing (part1, part2)

import Array exposing (Array)
import Day1.Data exposing (data)


part1 =
    solution1 data


part2 =
    solution2 data


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
    if x == y then
        x
    else
        0


solution1 : String -> Int
solution1 str =
    let
        array =
            str |> toIntList |> Array.fromList

        lastResult =
            compareSame
                (Maybe.withDefault 0 (Array.get 0 array))
                (Maybe.withDefault 0 (Array.get (Array.length array - 1) array))
    in
    lastResult
        + (Array.foldr
            (\current tuple ->
                ( current, Tuple.second tuple + compareSame (Tuple.first tuple) current )
            )
            ( 0, 0 )
            array
            |> Tuple.second
          )


solution2 : String -> Int
solution2 str =
    let
        list =
            str |> toIntList

        halfLen =
            List.length list // 2
    in
    List.map2
        (\x y ->
            if x == y then
                x + y
            else
                0
        )
        (List.take halfLen list)
        (List.drop halfLen list)
        |> List.sum
