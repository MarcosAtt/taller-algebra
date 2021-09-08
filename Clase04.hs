sumatoriaN :: Int -> Int
sumatoriaN 0 = 0
sumatoriaN n = n + sumatoriaN (n-1)