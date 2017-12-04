import Word
import Number
import System.IO 
import System.Random
import Text.Read

-- the main function
main = do 
 displayHomePage
 tocredit

--get the total credits and display
tocredit = do 
 totalCredit <- getLineInttotal
 let totalc = totalCredit
 -- make sure the total credits is > 0
 if totalc <= 0
 then do
  putStrLn("  Your total credits should be more than 0 credit")
  tocredit
 else do
  -- display
  putStrLn("  You now have: $ " ++ show totalCredit ++ " credits." )
  gen1 <- getStdGen 
  onegame totalCredit gen1 0
 
 -- get one game bet amonut 
onegame totalcredits genX iter = do
 -- check if it is a reasonable number 
 oneGameCredit <- getLineIntsingle
 let onegamec = oneGameCredit
 if  onegamec > totalcredits 
 then do
  putStrLn("  You don't have enough credits!")
  onegame totalcredits genX iter
 else 
  if onegamec <= 0
  then do 
   putStrLn("  Please enter at least 1 credit to play!")
   onegame totalcredits genX iter
  else do
   putStrLn(" ")
   putStrLn(" ")
   putStrLn("  Now enter: ")
   putStrLn("    <spin>   to play")
   putStrLn("  <minigame> to play minigame")
   putStrLn("    <end>    to end the game") 
   option <- getLine
   play option totalcredits onegamec genX iter

displayHomePage = mapM_ putStrLn $
 "  ***********************************************************************":
 "  ************************* MAGIC SLOT MATCHINE *************************":
 "  ***********************************************************************":
 "":
 "":
 "                           Little insides:":
 "      This slot machine has 3 reels with 10 results on each reel":
 "        Prize money increase with the number of credits played":
 "                            1credit 2credit 3credit":
 "                777-777-ANY :  2       4       6":
 "                777-777-777 :  10      20      30":
 "            BOUNS-BOUNS-ANY :  2       4       6  + bonus mini game":
 "            Bonus mini game :  5       10      15":                  
 "            Mini game       :  2       4       6":
 "            etc....":
 "":
 "":
 "":
 "                             How to Play:":
 "                   Enter your credits and push spin!":
 "":
 "":
 []


-- the iteration function
play option totalCredit oneGameCredit genX iter
 -- no more money the game end
 | totalCredit <= 0 = do 
  putStrLn ("  You have no more credits")
  tocredit

 -- user's option to end the game
 | option == "end" = do
  putStrLn ("  Game Over")

 -- spin the reels
 | option == "spin" = do
  let newTotalCredit = totalCredit - oneGameCredit
  let olditer = iter
  -- the counter will set to zero after 5 round
  let iter = if olditer > 5 then 0 else olditer + 1

  -- build the reels based on number of times user played and the bet amount
  newReel <- buildMachine oneGameCredit iter

  let reel1list = newReel
  let reel2list = newReel
  let reel3list = newReel

  -- generat the spin result
  let (evalSpin1, gen2) = randomR (0, (length reel1list) - 1) genX:: (Int, StdGen)
  let (evalSpin2, gen3) = randomR (0, (length reel2list) - 1) gen2:: (Int, StdGen)
  let (evalSpin3, gen4) = randomR (0, (length reel3list) - 1) gen3:: (Int, StdGen)

  -- display the spin result
  let reel1result = reel1list !! evalSpin1
  let reel2result = reel2list !! evalSpin2
  let reel3result = reel3list !! evalSpin3
  putStrLn(" ")
  putStrLn ("  Reel1     Reel2     Reel3")
  putStrLn ("  " ++ reel1result ++ "      " ++ reel2result ++ "      " ++ reel3result)
  
  -- if spins two BOUNS then go to mini game
  gamecredit <- if reel1result == reel2result && reel1result == "BOUNS"
  then guessword genX
  else if reel2result == reel3result && reel2result == "BOUNS"
  then guessnumber genX
  else return(0)
   
  -- calculating the credits win/lose based on spin result
  let oldcredit = newTotalCredit
  let newTotalCredit = if reel1result == reel2result && reel2result == reel3result
                       then if gamecredit == 2
                            then oneGameCredit * 15 + oldcredit + oneGameCredit
                            else oneGameCredit * 10 + oldcredit + oneGameCredit
                       else if reel1result == reel2result || reel2result == reel3result || reel1result == reel3result
                            then if gamecredit == 2
                                 then oneGameCredit * 7 + oldcredit + oneGameCredit
                                 else oneGameCredit * 2 + oldcredit + oneGameCredit
                            else oldcredit


  -- display win amount
  putStrLn(" ")
  let winlosecredit = newTotalCredit - oldcredit - oneGameCredit
  if winlosecredit > 0
  then putStrLn("  You have won: $ " ++ show winlosecredit)
  else return()

 -- display total credits
  putStrLn("  You have: $ " ++ show newTotalCredit ++ " left.")
  if newTotalCredit == 0
  then play option 0 oneGameCredit genX iter
  else onegame newTotalCredit gen4 iter
  
  
  -- go to minigame
 | option == "minigame" = do
  let newTotalCredit = totalCredit - oneGameCredit
  let (n, gen1) = randomR (0, 1) genX:: (Int, StdGen)
  gamecredit <- if n == 0 then guessword genX else guessnumber genX
  
  -- calculate credits win/lose based on minigame result
  let oldcredit = newTotalCredit
  let newTotalCredit = if gamecredit == 2 then oldcredit + oneGameCredit*2 else oldcredit
  let winlosecredit = newTotalCredit - oldcredit
  if winlosecredit > 0
  then putStrLn("  You have won: $ " ++ show winlosecredit)
  else return()
  
  -- iterate play function
  putStrLn("  You have: $ " ++ show newTotalCredit ++ " left.")
  if newTotalCredit == 0
  then play "spin" 0 0 gen1 iter
  else onegame newTotalCredit gen1 iter

  
 -- if invaild input display options again
 | otherwise = do
  putStrLn(" ")
  putStrLn(" ")
  putStrLn("  Invaild Input")
  putStrLn("    Now enter: ")
  putStrLn("      <spin>   to play")
  putStrLn("    <minigame> to play minigame")
  putStrLn("      <end>    to end the game") 
  newoption <- getLine
  play newoption totalCredit oneGameCredit genX iter
  
 
 --- slot machine builder 
buildMachine oneGameCredit iter
 | (oneGameCredit*iter) == 1 = do
  return ["777","BOUNS","ORANGE","WATERMELON","BELL","PEACH","CHERRY","APPLE","PEAR"]
 | (oneGameCredit*iter) > 1 && oneGameCredit < 3 = do
  return ["777","BOUNS","ORANGE","ORANGE","BELL","PEACH","CHERRY","APPLE","PEAR"]
 | (oneGameCredit*iter) >= 3 && oneGameCredit < 99 = do
  return ["777","BOUNS","BOUNS","ORANGE","BELL","BELL","CHERRY","ORANGE","777","APPLE","PEAR"]
 | otherwise = do
  return ["777","BOUNS","ORANGE","WATERMELON","BELL","PEACH","CHERRY","APPLE","PEAR"]

-- input evaluator ask user to reinput if it is not numbers
getLineInttotal :: IO Int
getLineInttotal = do
 putStrLn(" ")
 putStrLn "  Now please enter the total credit you want to play:"
 line <- getLine
 case readMaybe line of
  Just x -> return x
  Nothing -> putStrLn "  Invalid number entered" >> getLineInttotal

  
-- input evaluator ask user to reinput if it is not numbers
getLineIntsingle :: IO Int
getLineIntsingle = do
 putStrLn(" ")
 putStrLn("---~~~---~~~---~~~---~~~---~~~---~~~---~~~---~~~---~~~---~~~---~~~---~~~---~~~---~~~---")
 putStrLn(" ")
 putStrLn "  Now please enter how many credits you want to bet on each game"
 line <- getLine
 case readMaybe line of
  Just x -> return x
  Nothing -> putStrLn "  Invalid number entered" >> getLineIntsingle
 
