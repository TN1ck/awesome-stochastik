-- P(X = n)
p :: Int -> Double
p n = 1/((fromIntegral n) + 1)

-- P(X > n)
p' :: Int -> Double
p' n =
        let
          range = [1..n]
          mapped = map p range
        in
          foldl1 (*) mapped

main = do
  let n = 2
  putStr $ "p(" ++ (show n) ++ ") = "
  putStrLn $ show $ p n
  putStr $ "p'(" ++ (show n) ++ ") = "
  putStrLn $ show $ p' n

  -- the results seem legit
