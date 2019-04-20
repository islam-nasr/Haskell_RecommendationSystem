dis :: Eq a => [a] -> [a]
data I = I String deriving (Eq, Show)
data U = U String deriving (Eq, Show)
dis [] = []
dis (x:xs) = if elem x xs then dis xs else x:dis xs
--dis (x:xs) = findonce x (dis(xs))
--dis (x:xs) = findonce x (dis(xs))
--findonce a [] = [a]
--findonce a (x:xs) = if(a==x) then (findonce a xs) else (x:findonce a xs)
--findonce a (x:xs) = if(a==x) then (findonce a xs) else (findonce a xs)++[x]


fromRatingsToItems :: Eq a => [(b,a,c)] -> [a]
fromRatingsToItems((b,a,c):xs) = dis (fromRatingsHelp ((b,a,c):xs))
fromRatingsHelp :: Eq a => [(b,a,c)] -> [a]
fromRatingsHelp [] = []
fromRatingsHelp ((b,a,c):xs) = (a:(fromRatingsHelp xs))

fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a]
fromRatingsToUsers((a,b,c):xs) = dis (fromRatingsUserHelp ((a,b,c):xs))
fromRatingsUserHelp :: Eq a => [(a,b,c)] -> [a]
fromRatingsUserHelp [] = []
fromRatingsUserHelp ((a,b,c):xs) = (a:(fromRatingsUserHelp xs))



hasRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
hasRating x y [] = False
hasRating x y ((a,b,c):xs)= if x == a && y == b then True else hasRating x y xs


getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRating x y ((a,b,c):xs)= if hasRating x y ((a,b,c):xs) == True then getRatingHelp x y ((a,b,c):xs) else error "No given rating"
getRatingHelp :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRatingHelp x y ((a,b,c):xs)= if x == a && y == b then c else getRatingHelp x y xs

data Rating a = R a | NoRating deriving(Show,Eq)
formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c]
formMatrixUser _ [] _ = []
formMatrixUser x (y:ys) ((a,b,c):xs)= if hasRating x y ((a,b,c):xs) then (R (getRating x y ((a,b,c):xs))) :  formMatrixUser x ys ((a,b,c):xs) 
else NoRating : formMatrixUser x ys ((a,b,c):xs) 


formMatrix :: (Eq a, Eq b, Fractional c) => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]
formMatrix [] _ _= []
formMatrix (x:xs) (y:ys) ((a,b,c):zs)= formMatrixUser x (y:ys) ((a,b,c):zs) : formMatrix xs (y:ys) ((a,b,c):zs)

numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b
numberRatingsGivenItem a [] = 0
numberRatingsGivenItem a ((y:ys):xs)= if (y:ys)!!a == NoRating then numberRatingsGivenItem a xs else 1 + numberRatingsGivenItem a xs

differeneRatings :: Fractional a => Rating a -> Rating a -> a
differeneRatings NoRating _ =0
differeneRatings _ NoRating =0
differeneRatings (R a) (R b)= a-b

--matrixPairs :: Num a => a -> [(a,a)] 
--matrixPairs 0 =[]
--matrixPairs a = matrixPairsHelper a 0
--matrixPairsHelper a b= if b<a then range((b,0),(b,a)) ++matrixPairsHelper a (b+1) else []
--matrixPairsHelper a b =ranges (b,b) (a,a)
--ranges (lo1,hi1) (lo2,hi2) if hi1 == lo2 then ranges ((lo1,hi2) : rest) 
--ranges (interval:rest) = interval : ranges rest
--ranges [] = []


matrixPairs :: Num a => a -> [(a,a)]
matrixPairs a = matrixPairsHelper a 0 0
matrixPairsHelper2 a b c = if (a==(b+1) && a==c) then False else True
matrixPairsHelper a b c = if matrixPairsHelper2 a b c then (if c/=a then [(b,c)]++ (matrixPairsHelper a b (c+1)) else matrixPairsHelper a (b+1) 0) else []


dMatrix :: Fractional a => [[Rating a]] -> [a] 
dMatrix (x:xs) = dMatrixHelper (x:xs) (matrixPairs (length x))
--length [] = 0
--length (x:xs) = 1+ length xs
dMatrixHelper _ [] = []
dMatrixHelper (x:xs) ((a,b):ys) = ((dMatrixHelper2 (x:xs) (a,b)): dMatrixHelper (x:xs) ys) 
dMatrixHelper2 [] _ = 0
dMatrixHelper2 ((y:ys):xs) (a,b) = (differeneRatings ((y:ys)!!a) ((y:ys)!!b)) + (dMatrixHelper2 xs (a,b))


freqMatrix :: (Num a, Fractional b) => [[Rating b]] -> [a]
freqMatrix (x:xs) = freqMatrixHelper (x:xs) (matrixPairs (length x))
freqMatrixHelper _ [] = []
freqMatrixHelper (x:xs) ((a,b):ys) = ((freqMatrixHelper2 (x:xs) (a,b)): (freqMatrixHelper (x:xs) ys))
freqMatrixHelper2 [] _ = 0
freqMatrixHelper2 ((y:ys):xs) (a,b) = (differeneRatings2 ((y:ys)!!a) ((y:ys)!!b)) + (freqMatrixHelper2 xs (a,b))
differeneRatings2 NoRating _ =0
differeneRatings2 _ NoRating =0
differeneRatings2 (R a) (R b)=1


diffFreqMatrix :: Fractional a => [[Rating a]] -> [a]
diffFreqMatrix (x:xs) = helperDiffFreqMatrix (dMatrix (x:xs)) (freqMatrix (x:xs))
helperDiffFreqMatrix [] [] = []
helperDiffFreqMatrix (x:xs) (y:ys) = ((x/y):(helperDiffFreqMatrix xs ys))


predict (x:xs) a b= helper3 (helper (getRList (x:xs)) a b) (getRList (x:xs)!!a) (0,0)
	
helper (x:xs) a b = helper2  (matrixPairs (length x)) (diffFreqMatrix (x:xs)) (helper1 (x:xs) a b)


--gets positions needed from the pairs
helper1 (x:xs) a b=  pairsNeeded b ((x:xs)!!a)

helper2 _ _ [] =[]
helper2 ((a,b):xs) (y:ys) ((c,d):zs) = if (a==c && b==d) then y:(helper2 xs ys zs) else (helper2 xs ys ((c,d):zs))


helper3 [] [] (a,b) = a/b
helper3 (x:xs) (NoRating: ys) (a,b) =  helper3 xs ys (a,b) 
helper3 (x:xs) (R y:ys) (a,b) = helper3 xs ys ((x+y+a),(b+1))


--getRList ::[(Eq a, Eq b, Fractional c)] => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]
getRList (x:xs) =formMatrix (fromRatingsToUsers (x:xs)) (fromRatingsToItems (x:xs)) (x:xs)

--posofUserWithNo [] = 0
--posofUserWithNo (x:xs)= if elem NoRating x then 0 else 1+ posofUserWithNo xs

--posofNo [] =0
--posofNo (x:xs) = if x== NoRating then 0 else 1+ posofNo xs

pairsNeeded a (x:xs) = pairsNeededHelp a 0 (x:xs)
pairsNeededHelp _ _ [] = []
pairsNeededHelp a b (x:xs) = (a,b):pairsNeededHelp a (b+1) xs


