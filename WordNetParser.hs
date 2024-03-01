module WordNetParser where

import System.IO
import Control.Exception (try, catch, SomeException)


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
groupDefWN (h:t) llst = 
    if (checkH h)
        then (groupDefWN t (llst++[[h]]))
        else (groupDefWN t (reverse (concat h (reverse llst))))
        where
            checkH (' ':' ':' ':' ':_:' ':_:':':_) = True
            checkH (' ':' ':' ':' ':_:' ':_:_:':':_) = True
            checkH (' ':' ':' ':' ':_:_:_:' ':_:':':_) = True
            checkH (' ':' ':' ':' ':_:_:_:' ':_:_:':':_) = True
            checkH (' ':' ':' ':' ':_:':':_) = True
            checkH (' ':_) = False
            checkH ('E':_) = True
            checkH _ = True
            concat e (h:t) = (h++[e]):t



-- Define a function to handle receiving and storing responses from WordNet
storeResponses :: Handle -> [String] -> IO [String]
storeResponses handle lst = do
    response <- hGetLine handle
    -- putStrLn response
    if (checkNoMatch response) 
        then do 
            -- Test_Dict.main
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


-- Empty handle
emptyHandle :: Handle -> IO ()
emptyHandle handle = do
    response <- hGetLine handle
    if (checkCompleteCommand response)
        then do 
            return ()
        else do 
            emptyHandle handle



main :: Handle -> IO [[String]]
main handle = do 
    responses <- (storeResponses handle [""])
    if ((length responses) == 1 && (responses!!0) == "NoMatch")
        then do
            return [["NoMatch"]]
        else do
            let processedResponses = (drop 2 (reverse responses)) -- keep defs only
            let defLlst = (groupDefWN processedResponses [[[]]])
            -- empty the handle for future use
            emptyHandle handle
            return defLlst