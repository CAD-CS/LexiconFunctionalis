module Socket (main) where

import Pretty (prettyPrintGoodBye, prettyPrintAsk, prettyPrintAfterResponse, prettyPrintWelcome, prettyPrintWord)
import Network.Socket
import System.IO
import Control.Exception (catch, SomeException(SomeException))
import SocketHelpers
import qualified Control.Monad
import qualified WordRecommListReq
import qualified WordNetParser
import qualified GCideParser


-- | Main function
main :: IO ()
main = withSocketsDo $ do
    prettyPrintWelcome

    -- Establish connection and get handle
    (handle, database) <- handleConnection

    -- run the application w/o reestablishing connection to server
    run handle database

    -- Close the application and connection
    prettyPrintGoodBye
    hClose handle
    return ()



run :: Handle -> String -> IO ()
run handle db = do
    -- Enter the word to look up
    putStr "Enter the word to look up: "
    line <- getLine
    let newLine = fixdel line

    -- Get recommended word if potentially misspelt and confirm
    word <- WordRecommListReq.getRecommWord newLine

    -- Send a request to define a confimred word
    hPutStrLn handle $ "DEFINE " ++ db ++ " " ++ word
    hFlush handle

    llst <- if db == "wn" then WordNetParser.main handle
        else if db == "gcide" then GCideParser.main handle
        else error "Invalid databases at [run llst assignment]"

    if length llst == 1 && head (head llst) == "NoMatch"
        then do
            putStrLn "No Match. Please try again"
            run handle db
        else do
            prettyPrintWord word

            viewDef (drop 1 llst)

            prettyPrintAsk

            state <- stateListener
            putStrLn " "
            if state == "s" || state == "S"
                then run handle db
            else Control.Monad.when (state == "q" || state == "Q") $ return ()