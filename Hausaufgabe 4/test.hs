p :: Int -> Double
p n = 1/((fromIntegral n) + 1)

p' :: Int -> Double
p' n | n <= 1    = p 1
     | otherwise = (1 - (p' $ n-1)) * (p n)

prev :: Int -> Double
prev n =
        let
          nPlus = [1,2..(n-1)]
          mapped = map p' nPlus
        in
          foldl (+) 0 mapped

main = putStrLn $ show $ prev 50
