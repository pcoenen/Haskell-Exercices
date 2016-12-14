module Factory where
import Data.List (sort)

--Test Data

expensiveCarStation :: Station Resource Car
expensiveCarStation = machine [ ( Wheel , 4 ) , ( Paint , 2 ) , (BMWBody, 1 ) ]ExpensiveCar

cheapCarStation :: Station Resource Car
cheapCarStation = machine [ ( Wheel , 4 ) , ( Paint , 1 ) , ( IkeaBody , 1 ) ] CheapCar
 
factory = combine [expensiveCarStation , cheapCarStation]
resources1 = concat . replicate 5 $ replicate 6 Wheel ++ replicate 3 Paint ++ [BMWBody,IkeaBody]
resources2 = replicate 5 Paint ++ replicate 20 Wheel ++ replicate 5 IkeaBody
resources3 = replicate 6 Wheel ++ replicate 4 Paint ++ [BMWBody, IkeaBody ]

data Resource = Wheel | Paint | BMWBody | IkeaBody 
    deriving (Ord , Show , Eq)
data Car = ExpensiveCar | CheapCar | Bike
    deriving (Ord , Show , Eq)

--Exercise 1
data Station a b = M [(a, Int)] b | L [Station a b]
    deriving (Ord , Show , Eq)

machine :: [(a, Int)] -> b -> Station a b
machine = M

combine :: [Station a b] -> Station a b
combine = L

-- Exercise 2
trivial :: (Bounded a,Enum a) => Station a a
trivial = undefined
-- trivial = L [M [((toEnum x),1)] (toEnum x) | x <- [fromEnum (minBound :: a).. fromEnum (maxBound :: a)]] 

--Exercise 3
data Resources a = R [a]
    deriving (Ord , Show , Eq)

startResources :: Resources a
startResources = R []

amount :: Ord a => Resources a -> a -> Int
amount (R l) a = sum $ map (\x -> if x==a then 1 else 0) l

insert :: Ord a => Resources a -> a -> Resources a
insert (R l) a = R (a:l)

insertTimes (R l) a 0 = R l
insertTimes (R l) a n = insertTimes (R (a:l)) a (n-1)

--Exercise 4
run :: Ord a => [(a,Int)] -> b -> Resources a -> [a] -> ([a],[b])
run n b al [] 
    | filled n al   = ([],[b])
    | otherwise     = ([],[])
run n b al (x:xs)
    | filled n al               = (\(a,y) -> (a,b:y)) (run n b startResources (x:xs))
    | needed n x > amount al x  = run n b (insert al x) xs
    | otherwise                 = (\(a,y) -> (x:a,y)) (run n b al xs)



needed [] a = 0
needed ((x,n):l) a
    | x == a    = n
    | otherwise = needed l a

filled :: Ord a => [(a,Int)] -> Resources a -> Bool
filled [] _ = True
filled ((a,n):l) al
    | amount al a == n  = filled l al
    | otherwise         = False

--Exercise 5
runStation :: Ord a => Station a b -> [a] -> ([a],[b])
runStation (M x y) i    = run x y startResources i
runStation (L []) i     = (i,[])
runStation (L (x:xs)) i = (\ (o,(a,b)) -> (a,o++b)) $ (\ (a,b) -> (b, (runStation (L xs) a))) (runStation x i)
