module Day2.Solution exposing (part1, part2)

import Day2.Data exposing (data)


part1 =
    solution1 data


part2 =
    solution2 data


convert : String -> List (List Int)
convert str =
    let
        toIntDefault =
            String.toInt >> Result.withDefault 0
    in
    str
        |> String.trim
        |> String.split "\n"
        |> List.map (String.split " " >> List.map toIntDefault)


solution1 : String -> Int
solution1 str =
    let
        list =
            convert str

        minList =
            List.map (List.minimum >> Maybe.withDefault 0) list

        maxList =
            List.map (List.maximum >> Maybe.withDefault 0) list

        diffs =
            List.map2 (\x y -> x - y) maxList minList
    in
    List.sum diffs


divisible : ( Int, Int ) -> Bool
divisible ( x, y ) =
    rem x y == 0


makePairs : List a -> List ( a, a )
makePairs list =
    List.concatMap
        (\x ->
            List.map
                (\y -> ( x, y ))
                list
                |> List.filter (\( x, y ) -> x /= y)
        )
        list


findDivisible : List Int -> ( Int, Int )
findDivisible list =
    List.filter (\pair -> divisible pair) (makePairs list)
        |> List.head
        |> Maybe.withDefault ( 1, 1 )


solution2 : String -> Int
solution2 str =
    let
        list =
            convert str

        dividePair =
            \( x, y ) -> x // y
    in
    list
        |> List.map (findDivisible >> dividePair)
        |> List.sum
