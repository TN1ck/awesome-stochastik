p n = 1/(fromIntegral n)

p' n | n <= 1    = p 1
     | otherwise = (1 - (p' $ n-1)) * (p n)

main = putStrLn $ show $ p' 20