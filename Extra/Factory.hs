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
data Station a b = M [(a,Int)] b | L [Station a b]
    deriving (Eq, Ord, Show)

machine :: [(a, Int)] -> b -> Station a b
machine = M

combine :: [Station a b] -> Station a b
combine = L

-- Exercise 2
trivial :: (Bounded a,Enum a) => Station a a
trivial = combine $ map (\x -> machine [(x,1)] x) $ enumFromTo minBound maxBound

--Exercise 3
data Resources a = R [(a,Int)]

startResources :: Resources a
startResources = R []

amount :: Ord a => Resources a -> a -> Int
amount (R list) r = case lookup r list of
                        Just v -> v 
                        Nothing -> 0

insert :: Ord a => Resources a -> a -> Resources a
insert (R []) r = R [(r,1)]
insert (R ((e,a):ls)) r
    | e == r = R ((e,a+1):ls)
    | otherwise = do
                    let R nl = insert (R ls) r
                    R ((e,a):nl)

delete :: Ord a => Resources a -> a -> Int -> Resources a
delete (R ((k,v):ls)) r a
    | k == r = if v-a == 0
                then R ls
                else R ((k,v-a):ls)
    | otherwise = do
                    let R nl = delete (R ls) r a
                    R ((k,v):nl)

deleteAll :: Ord a => [(a,Int)] -> Resources a -> Resources a
deleteAll [] rlist = rlist
deleteAll ((r,a):xs) rlist = deleteAll xs (delete rlist r a)

--Exercise 4
run :: Ord a => [(a,Int)] -> b -> Resources a -> [a] -> ([a],[b])
run neededR product currentR [] 
    |enough neededR currentR    = (\(a,b) -> (a,product:b)) $ run neededR product (deleteAll neededR currentR) []
    |otherwise = ([],[])
run neededR product currentR (r:rs)
    |enough neededR currentR    = (\(a,b) -> (a,product:b)) $ run neededR product (deleteAll neededR currentR) (r:rs)
    |needed neededR currentR r  = run neededR product (insert currentR r) rs
    |otherwise                  = (\(a,b) -> (r:a,b)) $ run neededR product currentR rs

enough :: Ord a => [(a,Int)] -> Resources a -> Bool 
enough [] _ = True
enough ((r,a):ls) currentR
    |amount currentR r == a = enough ls currentR
    |otherwise = False 

needed :: Ord a => [(a,Int)] -> Resources a -> a -> Bool 
needed neededR currentR r = case lookup r neededR of
                                Just x -> if x > amount currentR r
                                            then True
                                            else False
                                Nothing -> False
                        

--Exercise 5
runStation :: Ord a => Station a b -> [a] -> ([a],[b])
runStation (M neededR product) inputR = run neededR product startResources inputR
runStation (L []) inputR = (inputR,[])
runStation (L (m:ms)) inputR = do
                                let (a,b) = runStation m inputR
                                let (an,bn) = runStation (L ms) a
                                (an,b ++ bn)
