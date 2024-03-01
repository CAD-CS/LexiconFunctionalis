{-
The code below is a Haskell version of Norvig's spelling corrector
Ref:
https://norvig.com/spell-correct.html
https://github.com/cbeav/symspell/blob/master/resources/frequencies.txt
https://www.olivierverdier.com/posts/2015/02/07/Haskell-spelling/
-}
-- Purpose and Usage:
-- To recommend at most top 4 possible corrections for a misspelt word.
-- In the main application, check if an entered word exist in the big.tex.
-- If so, don't run this recommender and proceed to search. 
-- If not found, run it and display at most 4 choices to users, including the origianl misspelt word.

module WordRecommListCtor where

import Data.Char (toLower)
import qualified Data.Map.Lazy as Map
import Data.List (nub)

-- ############################################################################
-- ###  Return a recommended list of up to 4 words using the misspelt word  ###
-- ###             Run buildRecommList in your primary module               ###
-- ############################################################################
buildRecommList :: String -> IO [[Char]]
buildRecommList word = do 
    wordMap <- wordCounter
    if (Map.member word wordMap)
        then return [word]
        else do
            ret <- recomm word wordMap
            return ret


-- editors
splits :: [Char] -> [([Char], [Char])]
splits lst = zip (reverse (makeLst1 (map toLower lst))) (makeLst2 (map toLower lst))
    where
        makeLst1 [] = [[]]
        makeLst1 lst = lst:(makeLst1 (init lst))
        makeLst2 [] = [[]]
        makeLst2 (h:t) = (h:t):(makeLst2 t)
     
deletes :: [Char] -> [[Char]]
deletes lst = [ x++t | (x, (h:t)) <- (splits lst), (h:t)/=[]]

transposes :: [Char] -> [[Char]]
transposes lst = [ x++(b:a:t) | (x, (a:b:t)) <- (splits lst), (length (a:b:t)) > 1]

replaces :: [Char] -> [[Char]]
replaces lst = foldr (\x v -> if (length (fst x) > 0) then ([(init (fst x)) ++ [c]++ (snd x)| c <- ['a'..'z']]++v) else v) [[]] (splits lst)

inserts :: [Char] -> [[Char]]
inserts lst = foldr (\x v -> if (length (fst x) >= 0) then ([(fst x) ++ [c]++ (snd x)| c <- ['a'..'z']]++v) else v) [[]] (splits lst)


-- All edits that are one edit away from input lst
edits1 :: [Char] -> [[Char]]
edits1 lst = ((deletes lst)++(transposes lst)++(replaces lst)++(inserts lst))


-- All edits that are two edits away from input lst
edits2 :: [Char] -> [[Char]]
edits2 lst = foldr (\x v -> (edits1 x)++v) [[]] (edits1 lst)

-- convert String to Integer
stringToInteger :: String -> Integer
stringToInteger str = read str


-- Function to create a map of (word, frequency) 
makeMap :: [String] -> (Map.Map String Integer)
makeMap [] = Map.empty
makeMap (k:v:xs) = Map.insert k (stringToInteger v) (makeMap xs)


-- Read file and count word frequency
wordCounter :: IO (Map.Map String Integer)
wordCounter = do
    file <- readFile "frequencies.txt"
    let content = words file
    -- putStrLn (show (length content))
    let wordFreqMap = makeMap content
    return wordFreqMap

-- Converts Just a to a, and Nothing to 0
convertMaybe :: (Integral a) => Maybe a -> a
convertMaybe (Just a) = a
convertMaybe Nothing  = 0

-- Calculate the Probability of a given input
prob :: [Char] -> (Map.Map String Integer) -> Integer
prob w dict = ((convertMaybe v))
    where 
        v = Map.lookup w dict

-- Helper: include word if found in frequencies.txt
known :: [[Char]] -> (Map.Map String Integer) -> [[Char]]
known lst dict = filter (\x -> if (Map.member x dict) then True else False) lst

-- Generate possible choices for potentially misspelt word.
candidates :: [Char] -> (Map.Map String Integer) -> [[Char]]
candidates lst dict
    |(length (known (edits1 lst) dict) /= 0) = known (edits1 lst) dict
    |otherwise = known (edits2 lst) dict



-- replace: replace an old element with a new one in a list
replace :: [Char] -> [Char] -> [[Char]] -> [[Char]]
replace t r (x:xs)
    |(t==x) = r:xs
    |otherwise = x: (replace t r xs)

-- find what to replace
searchReplace :: Int -> Int -> Int -> [[Char]] -> [[Char]] -> [[Char]]
searchReplace i low high llst temp = do
    if (i<=high)
        then do 
            let target = (llst!!i)
            let replacement = (temp!!(i-low))
            let update = replace target replacement llst
            searchReplace (i+1) low high update temp
        else llst


    
-- recursive merge helper
recurM :: Int -> Int -> Int -> Int -> Int -> Int -> [[Char]] -> [[Char]] -> (Map.Map String Integer) -> IO [[Char]]
recurM a b k low mid high llst temp dict
    |(k <= high) = do
        if (a <= mid && (b > high || (prob (llst!!a) dict) < (prob (llst!!b) dict)))
            then do
                let newTemp = (llst!!a) : temp
                recurM (a+1) b (k+1) low mid high llst newTemp dict
            else do
                let newTemp = (llst!!b) : temp
                recurM a (b+1) (k+1) low mid high llst newTemp dict
    |otherwise = do
        let i = low
        return (searchReplace i low high llst temp)


-- merge the sorted
merge :: [[Char]] -> Int -> Int -> Int -> [[Char]] -> (Map.Map String Integer) -> IO [[Char]] 
merge llst low mid high temp dict = do
    let a = low
    let b = mid + 1
    let k = low
    merged <- recurM a b k low mid high llst temp dict
    return merged

    
-- mSort: sort left and right
mSort :: [[Char]] -> Int -> Int -> [[Char]] -> (Map.Map String Integer) -> IO [[Char]]
mSort llst low high temp dict = do
    if (low < high) 
        then do
            let mid = (low + high) `div` 2
            _ <- mSort llst low mid temp dict
            _ <- mSort llst (mid+1) high temp dict
            merged <- merge llst low mid high temp dict
            return merged
        else return []


-- Merge Sort
mergeSort :: [[Char]] -> (Map.Map String Integer) -> IO [[Char]] 
mergeSort llst dict = do
    ret <- mSort llst 0 ((length llst) -1) [[]] dict
    return ret


-- Recommend top 4 possible choices from the input word
recomm :: [Char] -> (Map.Map String Integer) -> IO [[Char]]
recomm word dict = do
    let candidatesLlst = candidates word dict
    ret <- mergeSort candidatesLlst dict
    return (take 4 (reverse (nub ret)))


printRes :: [String] -> IO ()
printRes [] = return ()
printRes (h:t) = do 
    if (length (h:t) /= 0)
        then do
            putStrLn h 
            printRes t 
        else return ()



play :: (Map.Map String Integer) -> IO ()
play dict = do
    putStrLn "Enter a word"
    line <- getLine
    -- putStrLn (show (length f))
    g <- recomm line dict
    let res = "you entered " ++ line++". Do you mean: "
    putStrLn res
    printRes g
    -- putStrLn (g!!3)
    play dict

main :: IO ()
main = do 
    dict <- wordCounter
    play dict
    -- putStrLn (show (Map.lookup "|not" a))
    -- let b = prob "not" a
    -- putStrLn (show b)
    -- let c = Map.toList a 
    -- putStr (show (snd (c!!8)))
    -- let d = known ["qwhiodw", "not"] a 
    -- putStrLn (show (length d))
    -- let e = candidates "aple" dict 
    -- putStrLn (show (length e))
    -- putStrLn (head e) 
    -- line <- getLine
    -- let f = candidates line dict 
    -- -- putStrLn (show (length f))
    -- g <- recomm f dict
    -- putStrLn "you entered aple. Do you mean: "
    -- putStrLn (g!!0)
    -- putStrLn (g!!1)
    -- putStrLn (g!!2)
    -- putStrLn (g!!3)
    -- putStrLn (head e) 
    
    

