import System.IO 
import System.Random

main = do 
 handle <- openFile "words.txt" ReadMode
 contents <- hGetContents handle
 gen <- getStdGen
 let words = lines contents
 let (n, _ ) = randomR (0, (length words) - 1)  gen :: (Int, StdGen)
 let word = words !! n
 play word ( map (\ x -> '_') word ) 6
 hClose handle

play word known guesses
 | word == known = do
 	putStrLn known
	putStrLn ("You win!")
 | guesses == 0 = do
	putStrLn known
	putStrLn ("You lose. The word was " ++ word ++ ".")
 | otherwise = do
  	putStrLn known
	putStrLn ("You have " ++ (show guesses) ++ " guesses left.")
	line <- getLine
	let (newKnown, newGuesses) = handle (head line) word known guesses
	play word newKnown newGuesses


handle letter word known guesses
 | letter `elem` word = ( zipWith (\ w k -> if w == letter then w else k) word known, guesses)
 | otherwise = (known, guesses - 1)