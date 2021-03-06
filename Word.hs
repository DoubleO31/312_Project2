module Word where
import System.IO 
import System.Random
import Data.Char


guessword genX= do 
 displayHelp
 handle <- openFile "words.txt" ReadMode
 contents <- hGetContents handle
 let words = lines contents
 let (n, _ ) = randomR (0, (length words) - 1)  genX :: (Int, StdGen)
 let word = words !! n
 playguessword word ( map (\ x -> '_' ) word ) 6


displayHelp = mapM_ putStrLn $
 "":
 "":
 "  ***********************************************************************":
 "  ************************ GUESS THE MAGIC WORLD ************************":
 "  ***********************************************************************":
 "":
 "":
 "                           Little insides:":
 "                 You have 6 chances to guess the word":
 "                   The word are slot machine related ":
 "":
 "":
 "":
 "                             How to Play:":
 "                       Type your letter and Enter!":
 "":
 "                             LET'S START":
 "":
 "":
 []



playguessword word known guesses 
 | word == known = do
   putStrLn known
   putStrLn ("  You win!")
   return 2
 | guesses == 0 = do
   putStrLn known
   putStrLn ("  You lose. The word was " ++ word ++ ".")
   return 0
 | otherwise = do
   putStrLn(" ")
   putStrLn ("  This is a " ++ (show(length known)) ++ " letters word.")
   putStrLn known
   putStrLn ("  You have " ++ (show guesses) ++ " guesses left.")
   putStrLn(" ")
   line <- getLine
   if length line == 0 
   then do playguessword word known guesses 
   else do
    let lowerline = map toLower line
    let (newKnown, newGuesses) = handle (head lowerline) word known guesses
    playguessword word newKnown newGuesses


handle letter word known guesses
 | letter `elem` word = ( zipWith (\ w k -> if w == letter then w else k) word known, guesses)
 | otherwise = (known, guesses -1)
