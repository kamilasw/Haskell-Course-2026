{-# LANGUAGE BangPatterns #-}
module Solution where


-- 3) sieve of erastosthenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:n) = p : sieve [x | x<-n, mod x p/=0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n = 
    if n<2 then False
    else elem n (primesTo n)

-- 1) goldbach pairs
goldbachPairs :: Int -> [(Int,Int)]
goldbachPairs n = [
    (p,q) |
    p <- [2..n],
    q <- [p..n],
    isPrime p,
    isPrime q,
    p+q == n
    ]

-- 2) co prime pairs
coprimePairs :: [Int]-> [(Int,Int)]
coprimePairs n = [
    (x,y) |
    x <- n,
    y <- n,
    x < y,
    gcd x y == 1
    ]


-- task 4) matrix multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul [] _ = []
matMul _ [] = []
matMul a b =
    [
        [
            sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1] ] |
            j<-[0 .. n-1]
        ] |
        i <- [0..m-1]
    ]
    where
        m = length a 
        p = length (head a)
        n = length (head b)

-- 5) permutations
permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs = [
    x : ys |
    i <- [0 .. length xs-1],
    let x = xs !! i,
    let rest = take i xs ++ drop (i+1) xs,
    ys <- permutations (k-1) rest
    ]

    
-- 6) hamming numbers
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = 
    if x < y then x : merge xs (y:ys)
    else 
        if x > y then y : merge (x:xs) ys
        else x : merge xs ys

hamming :: [Integer]
hamming = 1:rest
    where
        t2 = [2*x | x<-hamming]
        t3 = [3*x | x<-hamming]
        t5 = [5*x | x<-hamming]
        rest = merge t2 (merge t3 t5)

-- task 7) integer powerwith bang patterns


power :: Int -> Int -> Int
power b e = go 1 e
    where 
        go :: Int -> Int -> Int
        go !acc 0 = acc
        go !acc n = go (acc*b)(n-1)


-- task 8) running maximum

-- using seq
listMax :: [Int] -> Int
listMax [] = error "empty list"
listMax (x:xs) = helper x xs
    where
        helper acc [] = acc
        helper acc (y:ys) = 
            let newacc = max acc y
            in seq newacc (helper newacc ys)

-- using bang patterns
listMaxv2 :: [Int] -> Int
listMaxv2 [] = error "empty list"
listMaxv2 (x:xs) = helper x xs
    where 
        helper !acc [] = acc
        helper !acc (y:ys) = helper (max acc y) ys



-- task 9) infinite prime stream
primes :: [Int]
primes = sieve [2..]

isPrimev2 :: Int -> Bool
isPrimev2 n = 
    n > 1 && 
    all nodiv (takeWhile small primes)
    where
        nodiv p = mod n p /= 0
        small p = p*p <=n


-- task 10) strict accumulation and space leaks

-- no strictness annotations
mean :: [Double] -> Double
mean [] = error "empty list"
mean xs = go 0 0 xs
    where
        go :: Double -> Int -> [Double] -> Double
        go s n [] = s / fromIntegral n
        go s n (x:xs) = go (s+x) (n+1) xs

-- bang pattern
meanv2 :: [Double] -> Double
meanv2 [] = error "empty list"
meanv2 xs = go 0 0 xs
    where
        go :: Double -> Int -> [Double] -> Double
        go !s !n [] = s / fromIntegral n
        go !s !n (x:xs) = go (s+x) (n+1) xs


-- mean and variance
meanv3 :: [Double] -> (Double,Double)
meanv3 [] = error "empty list"
meanv3 xs = go 0 0 0 xs
    where
        go :: Double -> Double -> Int -> [Double] -> (Double,Double)
        go !s !sq !n [] = 
            let m = s / fromIntegral n
                var = sq / fromIntegral n - m*m
            in (m,var)
        go !s !sq !n (x:xs) = go (s+x) (sq+x*x) (n+1) xs