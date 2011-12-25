import System.Random
import Ix

allowedChars = range ('A', 'Z') ++ range ('a', 'z') ++ range ('0', '9') ++ " ;"

randomiseList (xs) gen =
     let listLength = (length xs)-1
         (randomIndex, nextGen) = randomR (0, listLength) gen :: (Int, StdGen)
         in (xs !! randomIndex) : randomiseList xs nextGen

main = do
	gen <- getStdGen
	putStrLn $ take 5000 $ randomiseList allowedChars gen
