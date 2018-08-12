module Spring13.Week12.SubString
  ( findSubString
  ) where

findSubString :: [Char] -> [Char] -> Int
findSubString [] [] = 1
findSubString _ [] = -1
findSubString subStr (y:ys) 
    | length subStr > length (y:ys) = -1
    | length subStr > length ys = -1
    | otherwise = let isSubString = findIt subStr (y:ys) 
                  in if isSubString then 1 else findSubString subStr ys


findIt :: [Char] -> [Char] -> Bool
findIt (x:[]) (y:ys) = exists x y
findIt (x:xs) (y:ys) = 
  if (exists x y) then findIt xs ys 
  else findIt (x:xs) ys
findIt (x) [] = False

exists :: Char -> Char -> Bool
exists x y = (x == y || x == '*')
