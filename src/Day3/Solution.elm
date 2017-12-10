module Day3.Solution exposing (part1, part2)

import Day3.Data exposing (data)


part1 =
    solution1 data


part2 =
    0


solution1 : Int -> Int
solution1 address =
    address
        |> coordinates
        |> distanceToCenter


distanceToCenter : ( Int, Int ) -> Int
distanceToCenter ( x, y ) =
    abs x + abs y


coordinates : Int -> ( Int, Int )
coordinates address =
    let
        index =
            address - 1

        amplitude =
            getAmplitude address

        wavelength =
            getWavelength address

        x =
            discreteCos amplitude wavelength address

        y =
            discreteSin amplitude wavelength address
    in
    ( x, y )


getAmplitude : Int -> Int
getAmplitude address =
    ((sqrt (toFloat address) + 1) / 2 |> ceiling) - 1


getWavelength : Int -> Int
getWavelength address =
    let
        amplitude =
            getAmplitude address
    in
    if amplitude == 0 then
        1
    else
        amplitude * 8


discreteCos : Int -> Int -> Int -> Int
discreteCos amplitude wavelength x =
    let
        xMod =
            (x - 2) % wavelength

        sideLength =
            getSideLength wavelength
    in
    if xMod < (sideLength - 1) then
        amplitude
    else if xMod < (sideLength * 2 - 3) then
        -- linear decrease
        (sideLength * 2 - (4 + ((sideLength - 2) // 2))) - xMod
    else if xMod < (sideLength * 3 - 3) then
        -amplitude
    else if xMod < (sideLength * 4 - 5) then
        -- linear increase
        xMod - (sideLength * 4 - (6 + ((sideLength - 2) // 2)))
    else
        amplitude


discreteSin : Int -> Int -> Int -> Int
discreteSin amplitude wavelength y =
    let
        yMod =
            (y - 2) % wavelength

        sideLength =
            getSideLength wavelength
    in
    if yMod < (sideLength - 2) then
        -- linear increase
        yMod - (sideLength - (3 + ((sideLength - 2) // 2)))
    else if yMod < (sideLength * 2 - 2) then
        amplitude
    else if yMod < (sideLength * 3 - 4) then
        -- linear decrease
        (sideLength * 3 - (5 + ((sideLength - 2) // 2))) - yMod
    else
        -amplitude


getSideLength perimeter =
    (perimeter + 4) // 4



{--
x: 0 | 1 1 0 -1 -1 -1  0  1 |  2 2 2 2 1 0 -1 -2 -2 -2 -2 -2 -1  0  1  2 |  3  3 3 3 3 3
y: 0 | 0 1 1  1  0 -1 -1 -1 | -1 0 1 2 2 2  2  2  1  0 -1 -2 -2 -2 -2 -2 | -2 -1 0 1 2 3

  2  5  8
1 2  3  4  5
1 3  5  7  9
1 8 16 24 32
1 9 25 49

[ 1:(0, 0)

, 2:(1, 0), 3:(1, 1)
, 4:(0, 1), 5:(-1, 1)
, 6:(-1, 0), 7:(-1, -1)
, 8:(0, -1), 9:(1, -1)

, 10:(2, -1), 11:(2, 0), 12:(2, 1), 13:(2, 2)
, 14:(1, 2), 15:(0, 2), 16:(-1, 2), 17:(-2, 2)
, 18:(-2, 1), 19:(-2, 0), 20:(-2, -1), 21:(-2, -2)
, 22:(-1, -2), 23:(0, -2), 24:(1, -2), 25:(2, -2)

, 26:(3, -2), 27:(3, -1), 28:(3, 0), 29:(3, 1), 30:(3, 2), 31:(3, 3)
]
--}
