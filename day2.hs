-- Day 2 of AoC

readInt :: String -> Int
readInt = read

countOcc :: (Eq a) => a -> [a] -> Int
countOcc a [] = 0
countOcc a (x:xs) = (if a == x then 1 else 0) + (countOcc a xs)

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split cs delim

checkValid :: [String] -> Bool
checkValid tokens = do
    let range = map readInt (split (head tokens) '-')
    let occs = countOcc (head (head (tail tokens))) (head (tail (tail tokens)))
    if occs >= (head $ take 1 range) && occs <= (head $ drop 1 range) then True else False

countValid :: [String] -> Int
countValid [] = 0
countValid a = (if checkValid (take 3 a) then 1 else 0) + (countValid (drop 3 a))

main = do
    let file = "data/day2.data"
    contents <- readFile file
    print (countValid (words contents))

