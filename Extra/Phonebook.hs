module Phonebook (
  Name,
  PhoneNumber,
  Entry, mkEntry,name,phone,
  PhoneBook, names, phones, owner,
  Index(findEntry,empty,singleton,(<+>)),
  Assoc,
  byName,byPhone, emptyBook, addToBook, fromEntries,
  number, callerID,
  bill,bob,jeb,val,
  billbook,bobbook,jebbook,valbook,
  Lookup)
where  

import Data.List (intercalate)

-- - 1. Entry
type Name        = String
type PhoneNumber = [Int]

showPhone :: PhoneNumber -> String
showPhone = intercalate " " . map show

-- -- a. Complete the definitions of Entry, name and phone
data Entry = MkEntry Name PhoneNumber
  deriving (Eq,Show)

mkEntry :: Name -> PhoneNumber -> Entry
mkEntry = MkEntry

name :: Entry -> Name
name (MkEntry n _) = n

phone :: Entry -> PhoneNumber
phone (MkEntry _ p) = p


-- 2. Index

class Index i where
  findEntry :: Eq k => k -> i k -> Maybe Entry
  empty     :: Eq k => i k
  singleton :: Eq k => k -> Entry -> i k
  (<+>)     :: Eq k => i k -> i k -> i k

-- a. Complete the definition of Assoc
data Assoc k = MkAssoc [(k,Entry)]
  deriving (Eq,Show)

-- b. Complete the instance of Index for Assoc
instance Index Assoc where
  findEntry k (MkAssoc l) = lookup k l
  empty = MkAssoc []
  singleton k e = MkAssoc [(k,e)]
  (<+>) a (MkAssoc []) = a 
  (<+>) (MkAssoc a) (MkAssoc ((k,v):bs)) = case findEntry k (MkAssoc a) of
                                    Just x -> (<+>) (MkAssoc a) (MkAssoc bs)
                                    Nothing -> (<+>) (MkAssoc ((k,v):a)) (MkAssoc bs)

-- 3. Complete the definition of PhoneBook, names, phones and owner
data PhoneBook  = MkPhoneBook Entry (Assoc Name) (Assoc PhoneNumber) 
    deriving (Eq,Show)

names :: PhoneBook -> Assoc Name
names (MkPhoneBook _ n _)  = n

phones :: PhoneBook -> Assoc PhoneNumber
phones (MkPhoneBook _ _ p) = p

owner  :: PhoneBook -> Entry
owner (MkPhoneBook e _ _) = e
  
  
-- 4. Implement byName and byPhone, emptyBook, addToBook, fromEntries

byName :: Name -> PhoneBook -> Maybe Entry
byName n b = findEntry n $ names b

byPhone :: PhoneNumber -> PhoneBook -> Maybe Entry
byPhone p b = findEntry p $ phones b

emptyBook :: Entry -> PhoneBook
emptyBook e = MkPhoneBook e empty empty

addToBook :: Entry -> PhoneBook -> PhoneBook
addToBook e (MkPhoneBook o n p) = MkPhoneBook o (n <+> nn) (p <+> pn)
    where
        nn = singleton (name e) e
        pn = singleton (phone e) e

fromEntries :: Entry -> [Entry] -> PhoneBook
fromEntries o list = foldr addToBook (emptyBook o) list 

-- 5. Implement the callerID function.

data Telephone =
  MkTelephone PhoneNumber (PhoneNumber -> IO ())

number  :: Telephone -> PhoneNumber
number (MkTelephone pn _) = pn

receive :: Telephone -> PhoneNumber -> IO ()
receive (MkTelephone _ r) = r

callerID :: PhoneBook -> Telephone
callerID (MkPhoneBook e _ assoc) = MkTelephone (phone e) (\p -> function p assoc)

function :: PhoneNumber -> Assoc PhoneNumber -> IO ()
function p assoc = case findEntry p assoc of
                    Just x -> mapM_ putStrLn ["caller ID: " ++ name x, "Ring ring!"]
                    Nothing -> mapM_ putStrLn ["caller ID: " ++ showPhone p, "Ring ring!"]

-- 6. Calling someone

call :: PhoneBook -> [Telephone] -> IO ()
call (MkPhoneBook pn assoc _) list = do 
        putStrLn "Who would you like to call?"
        name <- getLine
        case findEntry name assoc of
            Just entry -> case findTelephone entry list of
                            Just t -> receive t (phone pn)
                            Nothing -> putStrLn "The number you dialed does not exist."
            Nothing -> putStrLn "No such entry!"

findTelephone :: Entry -> [Telephone] -> Maybe Telephone
findTelephone _ [] = Nothing
findTelephone entry (t:ts)
    |number t == phone entry    = Just t
    |otherwise                  = findTelephone entry ts

-- examples -- do NOT change

bill,bob,jeb,val :: Entry
bill = mkEntry "Bill"      [32,444,123]
bob  = mkEntry "Bob"       [32,444,124]
jeb  = mkEntry "Jebediah"  [32,444,125]
val  = mkEntry "Valentina" [32,444,126]

billbook,bobbook,jebbook,valbook :: PhoneBook
billbook = fromEntries bill [bob,jeb]
bobbook  = fromEntries bob  [bill,jeb]
jebbook  = fromEntries jeb  [bill,bob,val]
valbook  = fromEntries val  [bill,bob,jeb]

telephones :: [Telephone]
telephones = map callerID [billbook,bobbook,jebbook,valbook]

-- 7. Complete the Index instance for Lookup

data Lookup k = MkLookup (k -> Maybe Entry)

instance Index Lookup where
  findEntry key (MkLookup f) = f key
  empty = MkLookup (\x -> Nothing)
  singleton k e = MkLookup (\x -> lookup x [(k,e)])
  (<+>) (MkLookup f1) (MkLookup f2) = MkLookup (\x -> combine f1 f2 x)

combine f1 f2 k = case f1 k of
                    Just x -> Just x
                    Nothing -> f2 k
