import Guess
import System.IO 
import System.Random


main = do 
 displayHomePage
 tocredit

tocredit = do 
 putStrLn("Now please enter the total credit you want to play:")
 totalCredit <- readLn
 let totalc = totalCredit
 if totalc <= 0
 then do
  putStrLn("Your total credits should be more than 0 credit")
  tocredit
 else do
  putStrLn("You now have: $ " ++ show totalCredit ++ " credits." )
  gen1 <- getStdGen 
  onegame totalCredit gen1

onegame totalcredits genX = do
 putStrLn(" ")
 putStrLn("Enter how many credits you want to bet on each game")
 oneGameCredit <- readLn
 let onegamec = oneGameCredit
 if  onegamec > totalcredits 
 then do
  putStrLn("You don't have enough credits!")
  onegame totalcredits genX
 else if onegamec <= 0
      then do 
     putStrLn("Please enter at least 1 credit to play!")
     onegame totalcredits genX
    else return()


 putStrLn(" ")
 putStrLn(" ")
 putStrLn("Now enter: ")
 putStrLn("  <spin>   to play")
 putStrLn("<minigame> to play minigame")
 putStrLn("  <end>    to end the game") 

 option <- getLine
 play option totalcredits oneGameCredit genX 1

displayHomePage = mapM_ putStrLn $
 "***********************************************************************":
 "************************* MAGIC SLOT MATCHINE *************************":
 "***********************************************************************":
 "":
 "":
 "                           Little insides:":
 "      This slot machine has 3 reels with 10 results on each reel":
 "        Prize money increase with the number of credits played":
 "                            1credit 2credit 3credit":
 "                777-777-777 :  200     400     600":
 "          BOUNS-BOUNS-BOUNS :  100     200     300  + mini game ":
 "                               etc....":
 "":
 "":
 "":
 "                             How to Play:":
 "                   Enter your credits and push spin!":
 "":
 "":
 []



play option totalCredit oneGameCredit genX iter
 | totalCredit <= 0 = do 
  putStrLn ("You have no more credits")

 | option == "end" = do
  putStrLn ("Game Over")

 | option == "spin" = do
  let newTotalCredit = totalCredit - oneGameCredit
  let olditer = iter
  let iter = if olditer > 5 then 0
      else olditer + 1

  newReel <- buildMachine oneGameCredit iter

  let reel1list = newReel
  let reel2list = newReel
  let reel3list = newReel

  let (evalSpin1, gen2) = randomR (0, (length reel1list) - 1) genX:: (Int, StdGen)
  let (evalSpin2, gen3) = randomR (0, (length reel2list) - 1) gen2:: (Int, StdGen)
  let (evalSpin3, gen4) = randomR (0, (length reel3list) - 1) gen3:: (Int, StdGen)


  let reel1result = reel1list !! evalSpin1
  let reel2result = reel2list !! evalSpin2
  let reel3result = reel3list !! evalSpin3
  putStrLn(" ")
  putStrLn ("  Reel1     Reel2     Reel3")
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
  if newTotalCredit == 0
  then play option 0 oneGameCredit genX iter
  else onegame newTotalCredit gen4
  
  
 | option == "minigame" = do
  let newTotalCredit = totalCredit - oneGameCredit
  gamecredit <- guess genX
  let oldcredit = newTotalCredit
  let newTotalCredit = if gamecredit == 2
                       then oldcredit + oneGameCredit*2
             else oldcredit
  let winlosecredit = newTotalCredit - oldcredit
  if winlosecredit > 0
  then putStrLn("You have won: $ " ++ show winlosecredit)
  else return()

  putStrLn("You have: $ " ++ show newTotalCredit ++ " left.")
  if newTotalCredit == 0
  then play "spin" 0 0 genX iter
  else onegame newTotalCredit genX

  

 | otherwise = do
  putStrLn(" ")
  putStrLn(" ")
  putStrLn ("Invaild Input")
  putStrLn("Now enter: ")
  putStrLn("  <spin>   to play")
  putStrLn("<minigame> to play minigame")
  putStrLn("  <end>    to end the game") 
  option <- getLine  
  newoption <- getLine
  play newoption totalCredit oneGameCredit genX iter
  
  
  
  
buildMachine oneGameCredit iter
 | (oneGameCredit*iter) == 1 = do
  return ["777","BOUNS","ORANGE","WATERMELON","BELL","PEACH","CHERRY","APPLE","PEAR"]
 | (oneGameCredit*iter) > 1 && oneGameCredit < 3 = do
  return ["777","BOUNS","ORANGE","ORANGE","BELL","PEACH","CHERRY","APPLE","PEAR"]
 | (oneGameCredit*iter) >= 3 && oneGameCredit < 6 = do
  return ["777","BOUNS","BOUNS","ORANGE","BELL","BELL","CHERRY","ORANGE","777","APPLE","PEAR"]
 | otherwise = do
  return ["777","BOUNS","ORANGE","WATERMELON","BELL","PEACH","CHERRY","APPLE","PEAR"] 
