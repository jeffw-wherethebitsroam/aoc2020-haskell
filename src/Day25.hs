module Day25
  ( run,
  )
where

modexp :: Integer -> Integer -> Integer -> Integer
modexp _ 0 _ = 1
modexp b e m
  | even e = (r * r) `mod` m
  | otherwise = (b * r * r) `mod` m
  where
    r = modexp b (e `div` 2) m

find b e m x = if modexp b e m == x then e else find b (e + 1) m x

run :: IO ()
run = do
  let input = (9093927, 11001876)
  -- let input = (5764801, 17807724)

  let e1 = find 7 1 20201227 (fst input)
  let e2 = find 7 1 20201227 (snd input)
  print (e1, e2)

  let r = modexp 7 (e1 * e2) 20201227
  print r