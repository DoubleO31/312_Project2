-- putStrLn takes a string a returns an I/O action has 
--      a result type of () (the empty tuple also know as unit)

import System.IO 
import System.Random 

main = do 
 displayHomePage
 credit <- getLine
 putStrLn("Your credit is: $ " ++ credit )
 putStrLn("Now enter <spin> to play")
 option <- getLine
 -- result <- play option
 print(play option)
 -- putStrLn("Your spin: " ++ )



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
 "Now please enter the credit you want to play:":
 []


play option
 | option == "spin" = (evalSpin, evalSpin, evalSpin)
 | otherwise = (0,0,0)
 	where (evalSpin,_) = randomR (0, 9) getStdGen:: (Int, StdGen)
  -- return randNum




 -- evalSpin = do
 --  -- gen <- getStdGen
 --  let (randNum,_) <-  randomR (0, 9) getStdGen:: (Int, stdGen)
 --  return randNum

























