import Criterion.Main
import Day15

main =
  defaultMain
    [ bench "part1" $ whnf Day15.calc 2020
    --   bench "part2" $ whnf Day15.calc 30000000
    ]