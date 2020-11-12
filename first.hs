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


{--
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
<<<<<<< HEAD
--}

--test working on set equivalent for haskell
--also bit i does 1<<i I'm impressed
{--
import Data.Int
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Bits

fast_read_Int = fmap (map (fst . fromJust . BS8.readInt) . BS8.words) BS.getLine :: IO [Int]
fast_read_Int64 = fmap (map (fromIntegral . fst . fromJust . BS8.readInteger) . BS8.words) BS.getLine :: IO [Int64]

citesc_nr :: String -> Int64
citesc_nr s = read s

--insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
f :: Map.Map Int64 Int64 -> Int64 -> Map.Map Int64 Int64
f book x = Map.insertWith (+) x 1 book

main = do
    x <- getLine
    let n = citesc_nr x
    array <- fast_read_Int64
    let map_freq = foldl f Map.empty array:: Map.Map Int64 Int64
    print $ solve map_freq array

solve :: Map.Map Int64 Int64 -> [Int64] -> Int64
solve map_frec keys = (sum $ map calc' keys) `div` 2  where
    calc' keys = sum $ map (calc keys) [1..30] :: Int64
    calc x i 
        | complement <= 0 = 0 
        | complement `Map.notMember` map_frec = 0
        | complement == x = (map_frec Map.! complement) - 1
        | otherwise = map_frec Map.! complement
        where complement = bit i - x
--}


{--
import Data.Int
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
--import Data.HashMap.Strict as UMap cannot install it ;c
import qualified Data.Map.Strict as Map
import Data.Bits

fast_read_Int = fmap (map (fst . fromJust . BS8.readInt) . BS8.words) BS.getLine :: IO [Int]
fast_read_Int64 = fmap (map (fromIntegral . fst . fromJust . BS8.readInteger) . BS8.words) BS.getLine :: IO [Int64]

citesc_nr :: String -> Int64
citesc_nr s = read s

--insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
f :: Map.Map Int64 Int64 -> Int64 -> Map.Map Int64 Int64
f book x = Map.insertWith (+) x 1 book

main = do
    x <- getLine
    let n = citesc_nr x
    array <- fast_read_Int64
    let map_freq = foldl f Map.empty array:: Map.Map Int64 Int64
    print $ solve map_freq array

solve :: Map.Map Int64 Int64 -> [Int64] -> Int64
solve map_frec keys = (sum $ map calc' keys) `div` 2  where
    calc' keys = sum $ map (calc keys) [1..30] :: Int64
    calc x i 
        | complement <= 0 = 0 
        | complement `Map.notMember` map_frec = 0
        | complement == x = (map_frec Map.! complement) - 1
        | otherwise = map_frec Map.! complement
        where complement = bit i - x
--}

{--
citesc_nr :: String -> Int
citesc_nr s = read s
main = do
    x <- getLine
    let n = (citesc_nr x)
    y <- getLine
    let m = (citesc_nr y)    
    --print $ sum [m, n]
    putStrLn $ solve n m

solve :: Int -> Int -> String
solve a b 
    | f a + f b  == f x = "YES"
    | otherwise = "NO"
    where
        f = read . filter (/='0') . show  
        x = sum [a,b]
--}


{--
#include<bits/stdc++.h>
using namespace std;
int ap[26];
int main()
{
    string s;
    cin>>s;
    for(auto& i: s)
        ++ap[i-'a'];
    /*for(int i=0;i<26;++i)
    {
        if(ap[i]&1)
    }*/
    int i=0,j=25;
    while(i<j)
    {
        while(i<26 && !(ap[i]&1))
            ++i;
        while(j>=0 && !(ap[j]&1))
            --j;
        //cout<<(char)(i+'a')<<' '<<(char)(j+'a')<<endl;
        ++ap[i];
        --ap[j];
    }
    //cout<<ap[0]<<endl;
    //se termina in i ?
    string sol;
    for(int k=0;k<26;++k)
        for(int j=1;j<=ap[k]/2;++j)
            sol+=(k+'a');
    if(i<26 && ap[i]&1)
        sol+=(i+'a');
    for(int k=25;k>=0;--k)
        for(int j=1;j<=ap[k]/2;++j)
            sol+=(k+'a');
    cout<<sol;
}
--}

--incerc sa portez solutia in haskell
{--
import Data.List
import Data.Tuple
import Control.Monad
f x = ( snd x ) `mod` 2 == 1
g x = ( snd x ) `mod` 2 == 0

repl a b
	| fst a < fst b = [(fst a, snd a + 1), (fst b, snd b - 1)]
	| fst a == fst b = [(fst a, snd a - 1)]
	| otherwise = []

--multumesc stack overflow
repli (a,b) = replicate (b `div` 2) a

main = do
    x <- getLine
    --simuleaza map de frecventa
    let cs = map (\l -> (head l, length l)) . groupBy (\x y -> x == y) $ sortBy (\a b -> compare a b) x
    --let cs = sortBy (\a b -> compare a b) x
    --let cs = groupBy (\x y -> x == y) $ sort [1,2,3,2,1]
    let impare = filter f cs
    --let pare = filter (!f) cs nope nu merge
    let pare = filter g cs
    --indexul la map se face cu !!
    let mid = if odd (length impare) then [fst $ impare !! (length impare `div` 2)] else []
    let invers = reverse impare
    let delete_odds = zipWith repl impare invers
    let delete_zero = filter (\x -> snd x > 0) $ concat delete_odds
    let pare' = pare ++ delete_zero
    let pare_in_ordine = sort pare'
    let first_half = map repli pare_in_ordine
    --print first_half
    --print second_half
    let second_half = map repli $ reverse pare_in_ordine
    --print mid
    let answer = first_half ++ [mid] ++ second_half
    --let final = filter (/='0') . unwords answer
    let final = concat answer
    putStrLn final
--}


{--
import Control.Monad  
      
main = do   
    colors <- forM [1..5] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  
--}


{--
--got AC
import Data.Int
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import qualified Data.Map as M
fast_read_Int = fmap (map (fst . fromJust . BS8.readInt) . BS8.words) BS.getLine :: IO [Int]
fast_read_Int64 = fmap (map (fromIntegral . fst . fromJust . BS8.readInteger) . BS8.words) BS.getLine :: IO [Int64]

citesc_nr :: String -> Int64
citesc_nr s = read s
citesc_array :: [String] -> [Int64]
citesc_array = map read

--prefix sums
sumlist' xx = aux xx 0
    where aux [] a = []
          aux (x:xs) a = (a+x) : aux xs (a+x)


--pref m a = (a, if a `M.member` m then M.adjust succ a m else M.insert a 1 m)
f v x = Map.insertWith (+) x 1 v

solve :: Map.Map Int64 Int64 -> [(Int64,Int64)]
solve map_frec = M.toList map_frec

main = do  
    x <- getLine
    let n = (citesc_nr x)  
    vector <- fast_read_Int64
    let ps = sumlist' vector
    let dp = foldl f M.empty ps :: M.Map Int64 Int64
    print $ (n-) . foldl max 1 . map snd $ solve dp 
--}

--plz get AC
--https://codeforces.com/contest/550/problem/B
--simple quite beautiful 2^n solution
import Data.Int ( Int64 )
import Data.List ()
import Data.Maybe ( fromJust )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
 
fast_read_Int = fmap (map (fst . fromJust . BS8.readInt) . BS8.words) BS.getLine :: IO [Int] 
fast_read_Int64 = fmap (map (fromIntegral . fst . fromJust . BS8.readInteger) . BS8.words) BS.getLine :: IO [Int64]

check :: [Int64] -> Int64 -> Int64 -> Int64 -> Int64
check m l r x
    | l' >= 2 && total<=r && total>=l && dist >=x = 1
    | otherwise = 0
    where 
        dist = maximum m - minimum m
        total = sum m
        l' = length m

f :: [Int64] -> [Int64] -> Int64 -> Int64 -> Int64 -> Int64
f [] mask l r y = check mask l r y 
f (x:rest) mask l r y = (f rest mask l r y) + (f rest (x:mask) l r y)

main = do
    [n,l,r,x] <- fast_read_Int64
    asu <- fast_read_Int64
    print $ f asu [] l r x

