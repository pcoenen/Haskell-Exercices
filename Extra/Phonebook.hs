-- Pieter-Jan Coenen
-- r0584179
-- Informatica
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

import Data.List (intercalate,lookup)

-- - 1. Entry
type Name        = String
type PhoneNumber = [Int]

showPhone :: PhoneNumber -> String
showPhone = intercalate " " . map show

-- -- a. Complete the definitions of Entry, name and phone
data Entry
  = MkEntry Name PhoneNumber
  deriving (Eq,Show)

mkEntry :: Name -> PhoneNumber -> Entry
mkEntry = MkEntry

name :: Entry -> Name
name (MkEntry x _) = x

phone :: Entry -> PhoneNumber
phone (MkEntry _ y) = y 

-- 2. Index

class Index i where
  findEntry :: Eq k => k -> i k -> Maybe Entry
  empty     :: Eq k => i k
  singleton :: Eq k => k -> Entry -> i k
  (<+>)     :: Eq k => i k -> i k -> i k

-- a. Complete the definition of Assoc
data Assoc k
  = MkAssoc [(k,Entry)]
  deriving (Eq,Show)

-- b. Complete the instance of Index for Assoc
instance Index Assoc where
empty = MkAssoc []
findEntry _ (MkAssoc []) = Nothing
findEntry s (MkAssoc ((k,v):l))
    | s == k    = Just v
    | otherwise = findEntry s (MkAssoc l)

singleton k x = MkAssoc [(k,x)]
(<+>) (MkAssoc xs) (MkAssoc ((k,v):l)) = case findEntry k (MkAssoc xs) of
                                            Just q -> (<+>) (MkAssoc xs) (MkAssoc l)
                                            Nothing -> (<+>) (MkAssoc ((k,v):xs)) (MkAssoc l)
(<+>) x (MkAssoc []) = x 

-- 3. Complete the definition of PhoneBook, names, phones and owner
data PhoneBook = MkPhoneBook Entry (Assoc Name) (Assoc PhoneNumber) deriving (Eq,Show)

names :: PhoneBook -> Assoc Name
names (MkPhoneBook _ n _) = n

phones :: PhoneBook -> Assoc PhoneNumber
phones (MkPhoneBook _ _ p) = p

owner  :: PhoneBook -> Entry
owner (MkPhoneBook o _ _) = o
  
-- 4. Implement byName and byPhone, emptyBook, addToBook, fromEntries

byName :: Name -> PhoneBook -> Maybe Entry
byName n p = findEntry n (names p)

byPhone :: PhoneNumber -> PhoneBook -> Maybe Entry
byPhone n p = findEntry n (phones p)

emptyBook :: Entry -> PhoneBook
emptyBook e = MkPhoneBook e empty empty

addToBook :: Entry -> PhoneBook -> PhoneBook
addToBook e (MkPhoneBook i n p) = MkPhoneBook i (n <+> singleton (name e) e) (p <+> singleton (phone e) e) 

fromEntries :: Entry -> [Entry] -> PhoneBook
fromEntries o l = helperEntries (emptyBook o) l

helperEntries :: PhoneBook -> [Entry] -> PhoneBook
helperEntries b [] = b
helperEntries b (l:ls) = helperEntries (addToBook l b) ls

-- 5. Implement the callerID function.

data Telephone =
  MkTelephone PhoneNumber (PhoneNumber -> IO ())

number (MkTelephone pn _) = pn

receive :: Telephone -> PhoneNumber -> IO ()
receive (MkTelephone _ r) = r

callerID :: PhoneBook -> Telephone
callerID b = MkTelephone (phone $ owner b) (pf b)

pf :: PhoneBook -> PhoneNumber -> IO ()

pf b n = do
    case byPhone n b of
        Just r -> putStrLn ("caller ID: " ++ (name r))
        Nothing -> print n
    putStrLn "Ring ring !"

-- 6. Calling someone

call :: PhoneBook -> [Telephone] -> IO ()
call b t = do
        putStrLn "Who would you like to call"
        name <- getLine
        case byName name b of
            Just n -> case search (phone n) t of
                        Just q -> receive q (phone $ owner b) 
                        Nothing -> print "The number you dailed does not exist !"
            Nothing -> print "No such Entry"

search n [] = Nothing
search n (x:xs)
    | number x == n = Just x
    | otherwise     = search n xs

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
   --empty = MkLookup Nothing
    --findEntry s (MkLookup f) = f s

    --(<+>) (MkLookup x) (MkLookup y) = MkLookup (x.y)

