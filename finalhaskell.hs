changeFirst _ _ [] = []
changeFirst f a (x:xs) = if f x then [a]++(x:xs) else x: (changeFirst f a xs)

f :: [Int] -> [Int] -> Int


--helper :: [(Int,Int)] -> [(Int,Int) -> (Int,Int)]
f x y = foldr (+) 0 (map (\(x,y)->x) (filter (remainder) (zip x y)))


remainder (a,b) = rem a b ==0

