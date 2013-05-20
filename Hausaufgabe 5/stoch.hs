-- base function
p :: Int -> Double
p n = 1/((fromIntegral n) + 1)

-- P(X = n)
pEquals :: Int -> Double
pEquals n =
  let
    prev = [1..n-1]
    unsuccessful = if length prev > 0
      then
        foldl1 (*) $ map (\n -> 1-(p n)) [1..n-1]
      else
        1
    alltogether = unsuccessful * (p n)
  in
    alltogether

-- P(X <= n)
pLessThanOrEquals :: Int -> Double
pLessThanOrEquals n = foldl1 (+) $ map pEquals [1..n]

-- P(X > n)
pGreaterThan :: Int -> Double
pGreaterThan n = (-) 1 $ pLessThanOrEquals n

list n = map p [1..n]

main = do
  let n = 20
  --putStr $ "pLessThanOrEquals(" ++ (show n) ++ ") = "
  --putStrLn $ show $ pLessThanOrEquals n
  --putStr $ "pEquals(" ++ (show n) ++ ") = "
  --putStrLn $ show $ pEquals n
  putStrLn $ show $ list n


