module ParseInput where
import qualified Data.Char 
import qualified Data.List as DL 
import qualified Data.IntSet as S
import Data.IntSet (IntSet)

if' :: Bool -> p -> p -> p
if' True    x _ = x
if' False   _ y = y

leave:: Int -> [a] -> [a]
leave _ [] = []
leave n xs 
    |length xs > n  = reverse $ take n $ reverse xs
    |otherwise      = xs

reverseList:: [a] -> [a] -- turns "reverse" is already a function that does the same thing
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

-- like list1 `containsAll` list2
containsAll' :: Eq a =>  [a] -> [a] -> Bool
containsAll' _ [] = True 
containsAll' toBeChecked (y:ys) 
    |elem y toBeChecked = containsAll' (dropFirst y toBeChecked) ys
    |otherwise          = False

lower:: String -> String
lower = map Data.Char.toLower 

dropFirst :: Eq a => a -> [a] -> [a]
dropFirst e [] = []
dropFirst e (x:xs)
    | e == x    =  xs
    | otherwise = x : dropFirst e xs


dropElement :: Eq a => a -> [a] -> [a]
dropElement _ [] = []
dropElement y (x:xs)
    |y == x     = dropElement y xs
    |otherwise  = x : dropElement y xs


-- much nicer implementation, don't forget about discrete math 
dropElement' :: Eq a => a -> [a] -> [a]
dropElement' y ys = [x | x <- ys, x /= y]


split :: String -> (String, String)
split str = (trim' (takeWhile (/=' ') str), trim' (dropWhile (/=' ') str))

splitAtDel :: Eq a => a -> [a] -> [[a]]
splitAtDel el str = splitAtDelHelper el (trim el str) 

splitAtDelHelper :: Eq a => a -> [a] -> [[a]]
splitAtDelHelper el str =
    let (before, remainder) = span (/=el) str
      in case remainder of
        [] -> [before]
        x -> before : splitAtDelHelper el (tail x)


trim' :: String -> String
trim' = trim ' '

trim :: Eq a => a -> [a] -> [a]
trim el = dropWhile (==el) . DL.dropWhileEnd (==el)


createEdge :: (String,String) -> (String, String)
createEdge str
    |containsAll header (leave 4 tailer)    = (header, tailer)
    |otherwise                              = ("","")
    where 
        header = fst str 
        tailer = snd str

    --where 
    --    header = lower $ fst str
    --    tailer = lower $ snd str

createEdges :: [(String,String)] -> [(String, String)]
createEdges [] = []
createEdges (x:xs)
    | containsAll (fst x) (leave 4 (snd x)) = x : createEdges xs 
    | otherwise                             = createEdges xs


subsets :: (Eq t, Num t) => t -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs


subsetTuple :: [b] -> [(b, b)]
subsetTuple ys = [(x,y) | x <- ys, y <-ys] 



--containsAll toBeChecked theCheck = 
--    foldr step S.null theCheck toBeChecked
--    where
--        step c r toBeChecked = S.null toBeChecked || r (S.delete (fromEnum c) toBeChecked)

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll _ [] = True 
containsAll (x:xs) (y:ys) 
    |y `elem` (x:xs)    = containsAll (dropFirst y (x:xs)) ys
    |otherwise          = False

