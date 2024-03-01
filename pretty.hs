module Pretty (prettyPrintWord, prettyPrintWelcome, prettyPrintAfterResponse,prettyPrintAsk, prettyPrintGoodBye, prettyPrintInvalidInput, prettyPrintPrintDef, prettyPrintSwitchDef, prettyPrintSwitchDefListener) where
import System.Console.ANSI


--Print out welcome message
prettyPrintWelcome :: IO ()
prettyPrintWelcome = do
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]
    putStrLn " "
    putStrLn "$$$$$$$$$$$$$$$$$$$$$$$"
    putStr "$"
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
    putStr "------------------HELLO!---------------------"
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]
    putStrLn "$"
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
    putStr "----------Welcome to our Dictionary----------"
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red] 
    putStrLn "" 
    putStrLn "$$$$$$$$$$$$$$$$$$$$$$$"
    putStrLn " "
    setSGR [Reset]
    return ()


prettyPrintGoodBye :: IO ()
prettyPrintGoodBye = do 
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]
            putStrLn " "
            putStr "$"
            setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
            putStr "-------Thanks for using our Dictionary-------"
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]
            putStrLn "$"
            setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
            putStr "-----------------GoodBye!--------------------"
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]
            putStrLn " "
            setSGR [Reset]

prettyPrintAsk :: IO ()
prettyPrintAsk = do
    setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Green]
    putStrLn " "
    putStr "What would you like to do? Press s for a new search, or press q to quit"
    setSGR [Reset]

prettyPrintAfterResponse :: String -> IO ()
prettyPrintAfterResponse word = do
    putStrLn " "
    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity, SetItalicized True]
    putStrLn word
    setSGR [Reset]
    putStrLn " "

    

prettyPrintInvalidInput :: IO ()
prettyPrintInvalidInput = do
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]  -- Changing background color
            putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++++++++"
            putStrLn "+        Invalid input. Please try again.           +"
            putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++++++++"
            setSGR [Reset]


prettyPrintSwitchDef :: IO ()
prettyPrintSwitchDef = do 
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Green]
            putStrLn " "
            putStr "Press n for next or p for previous, or e to exit viewing: "
            setSGR [Reset]

prettyPrintPrintDef :: String -> IO ()
prettyPrintPrintDef h = do 
                setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Blue]  -- Changing background color
                putStrLn h
                setSGR [Reset]
            
prettyPrintSwitchDefListener :: IO () 
prettyPrintSwitchDefListener = do
            setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Red]  -- Changing background color
            putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
            putStrLn "+ Invalid input. Press n for next or p for previous, or e to exit viewing +"
            putStrLn "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
            setSGR [Reset]


prettyPrintWord :: String -> IO ()
prettyPrintWord word = do 
    putStrLn " "
    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity, SetItalicized True]
    putStrLn word 
    setSGR [Reset]
    putStrLn " "

