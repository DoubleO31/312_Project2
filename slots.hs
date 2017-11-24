-- putStrLn takes a string a returns an I/O action has 
--      a result type of () (the empty tuple also know as unit)

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

  let (evalSpin1, gen2) = randomR (0, 9) genX:: (Int, StdGen)
  let (evalSpin2, gen3) = randomR (0, 9) gen2:: (Int, StdGen)
  let (evalSpin3, gen4) = randomR (0, 9) gen3:: (Int, StdGen)
  let gen1 = gen4

  putStrLn ("reel 1: " ++ show evalSpin1)
  putStrLn ("reel 2: " ++ show evalSpin2)
  putStrLn ("reel 3: " ++ show evalSpin3)

  putStrLn(" ")
  putStrLn("You have: $ " ++ show newTotalCredit ++ " left.")
  putStrLn("Enter how many credits you want to bet on each game")
  newOneGameCredit <- readLn
  putStrLn("Now enter <spin> to play, <end> to end the game")
  newOption <- getLine
  play newOption newTotalCredit newOneGameCredit gen4



 | otherwise = do
  putStrLn ("Invaild Input")
