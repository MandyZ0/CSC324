-- | A polynomial.
poly :: Double -> Double
poly x = (x*x*x) - x*x - 2


-- | Tail recusive bisection method.
bisect :: (Double -> Double) -> Double -> Double -> Double -> Double
bisect f tol a b =
    let c = (a + b) / 2
        y = f c
    in
        if abs y < tol
        then
            c
        else
            if signum (f a) == signum y
            then
                bisect f tol c b
            else
                bisect f tol a c


-- | Stream-based implementation of the bisection method.
bisections :: (Double -> Double) -> Double -> Double -> [Double]
bisections f a b =
    let c = (a + b) / 2
        y = f c
    in
        if signum (f a) == signum y
        then
            c : bisections f c b
        else
            c : bisections f a c


findSmall :: Double -> (Double -> Double) -> [Double] -> Double
findSmall tol f nums =
    head $ filter (\x -> abs (f x) < tol) nums
