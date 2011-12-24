import System.Random
import Ix

randomText :: (Num a) => a -> a -> String
randomText _ _ = "Hello"

randomUpperCaseLetters gen = randomRs ('A','Z') gen

randomLowerCaseLetters gen = randomRs ('a', 'z') gen

randomDigits gen = randomRs ('0', '9') gen

randomMerge (x:xs) (y:ys) gen =
	let (randomBool, nextGen) = random gen :: (Bool, StdGen)
	in if randomBool
		then x:randomMerge xs ys nextGen
		else y:randomMerge xs ys nextGen


randomiseList (xs) gen =
	let listLength = (length xs)-1
	    (randomIndex, nextGen) = randomR (0, listLength) gen :: (Int, StdGen)
	in (xs !! randomIndex) : randomiseList xs nextGen

--randomElem (xs) gen =
--    let maxIdx = (length xs)-1
--        range = (0 :: Int, length xs)
--        randomIndex = randomR range gen :: (Integer, StdGen)
--    in (xs !! randomIndex)

main = do
	gen <- getStdGen
	let charList = range ('A', 'Z') : range ('a', 'z') : range ('0', '9') : " ;"
	putStrLn $ take 50 $ randomiseList charList gen
