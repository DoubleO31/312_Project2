import Guess
import System.IO 
import System.Random


main = do 
 displayHomePage
 totalCredit <- readLn
 putStrLn("Your total credits is: $ " ++ show totalCredit )
 gen1 <- getStdGen 
 onegame totalCredit gen1



onegame totalcredits genX = do
 putStrLn("Enter how many credits you want to bet on each game")
 oneGameCredit <- readLn
 let onegamec = oneGameCredit
 if onegamec > totalcredits
 then do
  putStrLn("You don't have enough credits!")
  onegame totalcredits genX
 else return()
 
 putStrLn("Now enter <spin> to play, <minigame> to play minigame, <end> to end the game")
 option <- getLine
 play option totalcredits oneGameCredit genX

displayHomePage = mapM_ putStrLn $
 "*****************************":
 "**** MAGIC SLOT MATCHINE ****":
 "*****************************":
 "":
 "How to Play:":
 "Enter your credits and play!":
 "":
 "Little insides:":
 "This slot machine has 3 reels with 10 results on each reel":
 "Prize money increase with the number of credits played":
 "       1credit 2credit 3credit":
 "1-1-1 :  200     400     600":
 "6-6-6 :  100     200     300":
 "etc....":
 "":
 "":
 "Now please enter the total credit you want to play:":
 []


play option totalCredit oneGameCredit genX
 | totalCredit <= 0 = do 
  putStrLn ("You have no more credits")

 | option == "end" = do
  putStrLn ("Game Over")

 | option == "spin" = do
  let newTotalCredit = totalCredit - oneGameCredit
  
  let reel1list = ["777","BAR","BAR","BAR","BAR","BAR","CHERRY","CHERRY","CHERRY"]
  let reel2list = ["777","BAR","BAR","BAR","BAR","BAR","CHERRY","CHERRY","CHERRY"]
  let reel3list = ["777","BAR","BAR","BAR","BAR","BAR","CHERRY","CHERRY","CHERRY"]

  let (evalSpin1, gen2) = randomR (0, (length reel1list) - 1) genX:: (Int, StdGen)
  let (evalSpin2, gen3) = randomR (0, (length reel2list) - 1) gen2:: (Int, StdGen)
  let (evalSpin3, gen4) = randomR (0, (length reel3list) - 1) gen3:: (Int, StdGen)


  let reel1result = reel1list !! evalSpin1
  let reel2result = reel2list !! evalSpin2
  let reel3result = reel3list !! evalSpin3
  putStrLn(" ")
  putStrLn ("Reel1   Reel2   Reel3")
  putStrLn ("  " ++ reel1result ++ "      " ++ reel2result ++ "      " ++ reel3result)
  
  gamecredit <- if reel1result == reel2result && reel2result == reel3result then guess genX
                else return(0)
  
  let oldcredit = newTotalCredit
  let newTotalCredit = if reel1result == reel2result && reel2result == reel3result
                       then oneGameCredit * 10 + oldcredit + gamecredit
                       else if reel1result == reel2result || reel2result == reel3result || reel1result == reel3result
                  then oneGameCredit*2 + oldcredit
              else oldcredit

  putStrLn(" ")
  let winlosecredit = newTotalCredit - oldcredit
  if winlosecredit > 0
  then putStrLn("You have won: $ " ++ show winlosecredit)
  else return()

  putStrLn("You have: $ " ++ show newTotalCredit ++ " left.")
  onegame newTotalCredit gen4
  
  
 | option == "minigame" = do
  let newTotalCredit = totalCredit - oneGameCredit
  gamecredit <- guess genX
  let oldcredit = newTotalCredit
  let newTotalCredit = if gamecredit == 1
                       then oldcredit + oneGameCredit*5
					   else oldcredit
  let winlosecredit = newTotalCredit - oldcredit
  if winlosecredit > 0
  then putStrLn("You have won: $ " ++ show winlosecredit)
  else return()

  putStrLn("You have: $ " ++ show newTotalCredit ++ " left.")
  onegame newTotalCredit genX

  

 | otherwise = do
  putStrLn ("Invaild Input")
  putStrLn("Now enter <spin> to play, <minigame> to play minigame, <end> to end the game")
  newoption <- getLine
  play newoption totalCredit oneGameCredit genX
