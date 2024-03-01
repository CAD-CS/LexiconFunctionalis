module GCideParser where

import System.IO
import Control.Exception (try, catch, SomeException)
import System.Console.ANSI


-- fixdel removes deleted elements from string
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r

checkCode :: String -> Bool
checkCode (a:b:c:r)
    |((a=='1') && (b=='5') && (c=='1')) = True
    |((a=='1') && (b=='5') && (c=='0')) = True
    |((a=='2') && (b=='2') && (c=='0')) = True
    |((a=='2') && (b=='5') && (c=='0')) = True
    |otherwise = False
checkCode _ = False

checkCompleteCommand :: String -> Bool
checkCompleteCommand (a:b:c:r)
    |((a=='2') && (b=='5') && (c=='0')) = True
    |otherwise = False
checkCompleteCommand (a:r)
    |(a =='.') = True
    |otherwise = False
checkCompleteCommand lst = False


checkNoMatch :: String -> Bool
checkNoMatch (a:b:c:r)
    |((a=='5') && (b=='5') && (c=='2')) = True
    |((a=='5') && (b=='0') && (c=='0')) = True
    |((a=='5') && (b=='0') && (c=='1')) = True
    |otherwise = False
checkNoMatch _ = False


-- Group the definitions of searched word from WordNet database 
groupDefWN :: [String] -> [[String]] -> [[String]]
groupDefWN [[]] llst = llst
groupDefWN [] llst = llst
groupDefWN (h:t) llst = do
    if (checkH h)
        then (groupDefWN t (llst++[[h]]))
        else (groupDefWN t (reverse (concat h (reverse llst))))
        where
            -- This checks for the rest of the definitions
            checkH (' ':' ':' ':' ':' ':' ':_) = False
            checkH (' ':' ':' ':a:'.':_)
                | isANumber a = True
                | otherwise = False

            checkH (' ':' ':' ':a:b:'.':_)
                | isANumber a && isANumber b = True
                | otherwise = False
            
            -- This checks for the first definition

            checkH (a:_)
                | a == ' ' = False
                | otherwise = True
            checkH (' ':' ':' ':_) = False

            checkH('_':_) = False
            checkH(_:_) = False
            checkH ('_':[]) = False

            isANumber c
                | '0' <= c && c <= '9' = True
                | otherwise = False
            concat e (h:t) = (h++[e]):t



-- Define a function to handle receiving and storing responses from WordNet
storeResponses :: Handle -> [String] -> IO [String]
storeResponses handle lst = do
    response <- hGetLine handle
    -- putStrLn (response)
    -- putStrLn response
    if (checkNoMatch response) 
        then do 
            -- Test_Dict.main
            -- putStrLn "NOMATCH for APP"
            return ["NoMatch"]
        else do 
            if (checkCompleteCommand response) 
                then return (("End of definitions"++response):lst)
                else do
                    if (checkCode response)
                        then do 
                            (storeResponses handle lst)
                        else do 
                            (storeResponses handle (response:lst))



checkCompleteCommandForEmptyingHandle :: String -> Bool
checkCompleteCommandForEmptyingHandle (a:b:c:r)
    |((a=='2') && (b=='5') && (c=='0')) = True
    |otherwise = False
checkCompleteCommandForEmptyingHandle lst = False


-- Empty handle
emptyHandle :: Handle -> IO ()
emptyHandle handle = do
    response <- hGetLine handle
    if (checkCompleteCommandForEmptyingHandle response)
        then do 
            return ()
        else do 
            emptyHandle handle



main :: Handle -> IO [[String]]
main handle = do 
    responses <- (storeResponses handle [""])
    if ((length responses) == 1 && (responses!!0) == "NoMatch")
        then do
            -- putStrLn "NOMATCH for APP"
            return [["NoMatch"]]
        else do
            let processedResponses = (drop 1 (reverse responses)) -- keep defs only
            -- putStrLn "processedResponses"
            -- putStrLn (processedResponses!!0)
            let defLlst = (groupDefWN processedResponses [[[]]])
            -- empty the handle for future use
            emptyHandle handle
            return defLlst