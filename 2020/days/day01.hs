import System.IO

main = do
    stringNumbers <- readFile "inputs/day01.txt" 
    let numbers = makeInteger $ lines $ stringNumbers
    let allCombinationsWithSum = [ (x,y,z, x+y+z) | x <- numbers, y <- numbers, z <- numbers]
    let sumIs2020 = filter (\(x,y,z,sum) -> sum == 2020) allCombinationsWithSum
    let res = sumIs2020 !! 0
    let prod = (first res) * (second res) * (third res)
    print prod
    return ()

makeInteger :: [String] -> [Int]
makeInteger stringNumbers = map read stringNumbers

first (x,_,_,_) = x
second (_,x,_,_) = x
third (_,_,x,_) = x
