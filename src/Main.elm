import Day1.Solution as Day1
import Day2.Solution as Day2
import Day3.Solution as Day3

import Html exposing (text, div)

main = showAnswers

showAnswers =
  div []
    [ showDay "1" Day1.part1 Day1.part2
    , showDay "2" Day2.part1 Day2.part2
    , showDay "3" Day3.part1 Day3.part2
    ]

showDay i part1 part2 =
  div []
    [ text ("Day " ++ i ++ ": " ++ (toString part1) ++ " -> " ++ (toString part2))
    ]
