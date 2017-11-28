--module Main where

import Guess
import System.IO 
import System.Random


main = do 
 displayHomePage
 totalCredit <- readLn
 putStrLn("Your total credits is: $ " ++ show totalCredit )
 putStrLn("Enter how many credits you want to bet on each game")
 oneGameCredit <- readLn

 putStrLn("Now enter <spin> to play, <end> to end the game")
 option <- getLine
 gen1 <- getStdGen

 play option totalCredit oneGameCredit gen1



displayHomePage = mapM_ putStrLn $
 "*****************************":
 "**** MAGIC SLOT MATCHINE ****":
 "*****************************":
 "":
 "How to Play:":
 "Play a credit(b/w 1-3) and spin!":
 "":
 "Little insides:":
 "This slot machine has 3 reels with 10 results on each reel":
 "Prize money increase linearly with the number of credits played":
 "       1credit 2credit 3credit":
 "1-1-1 :  200     400     600":
 "6-6-6 :  100     200     300":
 "etc....":
 "":
 "":
 "Now please enter the total credit you want to play:":
 []


play option totalCredit oneGameCredit genX
 | totalCredit == 0 = do 
  putStrLn ("You have no more credits")

 | option == "end" = do
  putStrLn ("Game Over")

 | option == "spin" = do
  let newTotalCredit = totalCredit - oneGameCredit

  let (evalSpin1, gen2) = randomR (0, 8) genX:: (Int, StdGen)
  let (evalSpin2, gen3) = randomR (0, 8) gen2:: (Int, StdGen)
  let (evalSpin3, gen4) = randomR (0, 8) gen3:: (Int, StdGen)

  let reel1list = ["7","\1046","\1046","\1046","\1069","\1069","\12398","\12398","\12398"]
  let reel2list = ["7","\1046","\1046","\1046","\1069","\1069","\12398","\12398","\12398"]
  let reel3list = ["7","\1046","\1046","\1046","\1069","\1069","\12398","\12398","\12398"]
  let reel1result = reel1list !! evalSpin1
  let reel2result = reel2list !! evalSpin2
  let reel3result = reel3list !! evalSpin3
  putStrLn(" ")
  putStrLn ("Reel1   Reel2   Reel3")
  putStrLn ("  " ++ reel1result ++ "      " ++ reel2result ++ "      " ++ reel3result)
  
  guess
  
  let oldcredit = newTotalCredit
  
  
  let newTotalCredit = if reel1result == reel2result && reel2result == reel3result
                       then oneGameCredit*10 + oldcredit
                       else if reel1result == reel2result || reel2result == reel3result || reel1result == reel3result
					        then oneGameCredit*2 + oldcredit
							else oldcredit

  putStrLn(" ")
  putStrLn("You have: $ " ++ show newTotalCredit ++ " left.")
  putStrLn("Enter how many credits you want to bet on each game")
  newOneGameCredit <- readLn
  putStrLn("Now enter <spin> to play, <end> to end the game")
  newOption <- getLine
  play newOption newTotalCredit newOneGameCredit gen4


 | otherwise = do
  putStrLn ("Invaild Input")
