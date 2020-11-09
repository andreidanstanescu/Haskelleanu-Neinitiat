--le me trying to write a reading function
{--import Control.Monad (mfilter)

--am facut citirea de mana si nu da rasp corect decat pt cifre
--upd acum mere
citesc_nr :: String -> Int
citesc_nr s = read s
citesc_array :: [String] -> [Int]
citesc_array = map read
main = do  
    x <- getLine
    let n = (citesc_nr x)  
    --putStrLn (x)  
    --vector <- read `fmap` getLine :: IO Int
    vector <- getLine
    --print vector
    --let i = (citesc_nr (takeWhile (/= '\n') vector) )
    --let y = filter (/=' ') vector
    let y = words vector

    --let replace a b = map $ maybe b id . mfilter (/= a) . Just
    --let y = replace ' ' '3' vector
    --let despart c = c:[]
    --let f x = map despart x
    --let array = f y
    let int_array = citesc_array y
    print $ solve int_array
    --print y

--interesting dynamic in haskelleanu

solve(x:xs) = dp xs x 1 1 where
    dp [] pred current ans = max current ans
    dp (curr:xs) pred current ans
        | curr > pred = dp xs curr (current+1) ans
        | otherwise = dp xs curr 1 (max current ans)
--}



{--main :: IO ()
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
      
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  
--}

--fmap se poate aplica oricarui functor (inca nu stiu ce e aia XD)
{--main = do
    x <- fmap length getLine :: IO Int 
    print x--}


{--citesc_nr :: String -> Int
citesc_nr  = read 
main = do
    [k2,k3,k5,k6] <- fmap (map citesc_nr . words) getLine :: IO [Int]
    print $  256 * foldl1 min [k2, k5, k6] + 32 * min k3 (k2 - foldl1 min [k2, k5, k6])
--}

import Control.Monad
import Data.Array
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
 
fast_read_Int = fmap (map (fst . fromJust . BS8.readInt) . BS8.words) BS.getLine :: IO [Int]   --nu e suficienta pt tipul de date(WA) din pb
fast_read_Int64 = fmap (map (fromIntegral . fst . fromJust . BS8.readInteger) . BS8.words) BS.getLine :: IO [Int64]


--cautare binara modificata pt valori egale

bs :: Array Int64 Int64 -> Int64 -> Int64 -> Int64
bs v n x = go 0 n where
    go st dr
        | st>=dr = dr - 1
        | v ! mid <= x = go (mid+1) dr
        | v ! mid > x = go st mid
        where mid = div (st + dr)  2


main = do
    [n,m,k] <- fast_read_Int64
    [x,s] <- fast_read_Int64
    a <- fmap (listArray (0,m-1)) fast_read_Int64  --bun fmapul asta
    b <- fmap (listArray (0,m-1)) fast_read_Int64
    c <- fmap (listArray (0,k-1)) fast_read_Int64
    d <- fmap (listArray (0,k-1)) fast_read_Int64
    let fara_1 = bs d k s 
    let initial = if fara_1 >= 0 then (max (n - c ! fara_1) 0) * x else n * x

    let f i = g i where
        g i
            | s < b ! i = maxBound :: Int64
            | otherwise = (max 0 (n - maxp)) * (a ! i)
            where 
                maxp = if binary_search >=0 then c ! binary_search else 0
                binary_search = bs d k (s- b ! i)


    let _for_ = minimum $ map f [0..m-1]
        
    print $ min initial _for_
