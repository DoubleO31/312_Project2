module Word where
import System.IO 
import System.Random

-- main function
guessword genX= do 
 displayHelp
 -- open the file that contains all words
 handle <- openFile "words.txt" ReadMode
 -- get the content of the file
 contents <- hGetContents handle
 -- make a list of words
 let words = lines contents
 -- randomly pick a word
 let (n, _ ) = randomR (0, (length words) - 1)  genX :: (Int, StdGen)
 let word = words !! n
 -- go to the iteration function
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


-- the iteration function
playguessword word known guesses 
 -- if the word matches then win
 | word == known = do
   putStrLn known
   putStrLn ("  You win!")
   return 2
 -- run out of guess chance the game end
 | guesses == 0 = do
   putStrLn known
   putStrLn ("  You lose. The word was " ++ word ++ ".")
   return 0
 -- display the game and iterate
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
    let (newKnown, newGuesses) = handle (head line) word known guesses
    playguessword word newKnown newGuesses

-- helper function zip the known letters together and display
handle letter word known guesses
 | letter `elem` word = ( zipWith (\ w k -> if w == letter then w else k) word known, guesses)
 | otherwise = (known, guesses -1)
