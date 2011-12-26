import System.Random
import Ix

--String of allowed characters to print
allowedChars = range ('A', 'Z') ++ range ('a', 'z') ++ range ('0', '9') ++ " ;"

--Takes a list and returns an infinite list of random elements from it
randomiseList (xs) gen =
    let listLength = (length xs)-1
        (randomIndex, nextGen) = randomR (0, listLength) gen :: (Int, StdGen)
        in (xs !! randomIndex) : randomiseList xs nextGen

--Take up to n elements from the given list
takeRandom :: Int -> [a] -> StdGen -> [a]
takeRandom n xs gen = 
    let (randNum, _) = randomR (1, n) gen :: (Int, StdGen)
    in take randNum xs

generateLine :: StdGen -> String
generateLine gen = takeRandom 200 (randomiseList allowedChars gen) gen

repeatLines :: (Num n) => n -> StdGen -> [String]
repeatLines 0 _ = []
repeatLines n gen =
    generateLine gen : repeatLines (n - 1) gen

repeatRandomLines :: Int -> StdGen -> [String]
repeatRandomLines n gen =
    let (randNum, _) = randomR (1, n) gen :: (Int, StdGen)
    in repeatLines randNum gen

generateLines :: StdGen -> [String]
generateLines gen = repeatRandomLines 20 gen

main = do
	gen <- getStdGen
	mapM_ putStrLn (generateLines gen)
