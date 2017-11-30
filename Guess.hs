module Guess where
import System.IO 
import System.Random


guess gen5= do 
 handle <- openFile "words.txt" ReadMode
 contents <- hGetContents handle
 let words = lines contents
 let (n, _ ) = randomR (0, (length words) - 1)  gen5 :: (Int, StdGen)
 let word = words !! n
 playguess word ( map (\ x -> '_') word ) 6

playguess word known guesses 
 | word == known = do
   putStrLn known
   putStrLn ("You win!")
   return 2
 | guesses == 0 = do
   putStrLn known
   putStrLn ("You lose. The word was " ++ word ++ ".")
   return 0
 | otherwise = do
   putStrLn known
   putStrLn ("You have " ++ (show guesses) ++ " guesses left.")
   line <- getLine
   let (newKnown, newGuesses) = handle (head line) word known guesses
   playguess word newKnown newGuesses


handle letter word known guesses
 | letter `elem` word = ( zipWith (\ w k -> if w == letter then w else k) word known, guesses-1)
 | otherwise = (known, guesses)
