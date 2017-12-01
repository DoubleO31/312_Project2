module Number where
import System.IO
import System.Random
import Text.Read


guessnumber genX= do
 let (randomnumber,_) = randomR (1, 100) genX:: (Int, StdGen)
 playguessnumber randomnumber 0 6

playguessnumber number guessn guesses 
 | number == guessn = do
   putStrLn ("You win!")
   return 2
 | guesses == 0 = do
   putStrLn ("You lose. The number was " ++ (show number) ++ ".")
   return 0
 | otherwise = do
   putStrLn ("You have " ++ (show guesses) ++ " guesses left.")
   line <- getLineInt
   handle number line guesses


handle number guessn guesses
 | guessn > number = do
  putStrLn(" ")
  putStrLn ("Your guess number is too high!")
  putStrLn(" ")
  let newguesses = guesses - 1
  playguessnumber number guessn newguesses
  
 | guessn == number = do
  playguessnumber number guessn guesses
  
 | otherwise = do
  putStrLn(" ")
  putStrLn ("Your guess number is too low!")
  putStrLn(" ")
  let newguesses = guesses - 1
  playguessnumber number guessn newguesses

  

getLineInt :: IO Int
getLineInt = do
  putStrLn "Now please enter a number ranging from 0 to 100:"
  line <- getLine
  case readMaybe line of
    Just x -> return x
    Nothing -> putStrLn "Invalid number entered" >> getLineInt
