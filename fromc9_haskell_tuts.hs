mylast :: [Int] -> Int
mylast [] = error "invalid array"
mylast (x:xs) | null xs   = x
            | otherwise = mylast(tail xs)


mylast1 :: [Int] -> Int
mylast1 xs = head (drop (length xs - 1) xs)

myInit :: [Int] -> [Int]
myInit xs = take (length xs - 1 ) xs

myInit1 :: [Int] -> [Int]
myInit1 [] = []
myInit1 (x:xs) | null xs = myInit1(xs)
               | otherwise = x : myInit1(xs)

safeTail :: [Int] -> [Int]
safeTail [] = []
safeTail ( x:xs) | null xs = [x]
                 | otherwise = xs
                 
pyths :: Int -> [(Int,Int,Int)]
pyths z = [(x,y,z) |x <- [1..z],y <- [1..z], x*x + y*y == z*z ]


factors :: Int -> [Int]
factors x = [y | y <- [1..(x-1)], mod x y == 0]


perfects :: Int -> [Int]
perfects x = [ y | y <- [1..x] , sum (factors y)  == y]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [ (fst p) * (snd p)| p <- zip xs ys]

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort small ++ [x] ++ quickSort big
                 where
                        small = [s | s <- xs , s < x]
                        big = [b | b <- xs , b > x]


merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = [x] ++ merge xs (y:ys)
                    | otherwise = [y] ++ merge (x:xs) ys


msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort (x:xs) = merge (sortPair [x,head xs]) (msort(tail xs))
    where
        sortPair (f:s) = if(f < (head s)) then (f:s) else reverse (f:s)
