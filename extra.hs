funA t = ((3-4*t),(2-3*t))

funB t = ((1-(t^2)),(t-2))

funC t = ((t^2+t),(t^2-t))

funD t = ((t^2),(t^3-4*t))

-- Sea An una sucesion
sucA 0 = 1
sucA 1 = 3
sucA 2 = 2
sucA 3 = (-1)
sucA 4 = (-3)
sucA 5 = (-2)
sucA n = sucA (n `mod` 6)

sumSucA 0 = 1
sumSucA x = sucA (x) + sumSucA (x-1)