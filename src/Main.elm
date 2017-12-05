import Day1.Solution as Day1
import Html exposing (text, div)

main = showAnswers

showAnswers =
  div []
    [ showDay Day1.part1 Day1.part2
    ]


showDay part1 part2 =
  div []
    [ text ("Day 1: " ++ (toString part1) ++ " -> " ++ (toString part2))
    ]
