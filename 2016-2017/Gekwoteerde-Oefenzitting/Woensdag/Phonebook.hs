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
mkEntry name ns = MkEntry name ns

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
data Assoc k
  = MkAssoc [(k,Entry)]
  deriving (Eq,Show)

-- b. Complete the instance of Index for Assoc
instance Index Assoc where
  -- complete this instance
  empty = MkAssoc []
  singleton k e = MkAssoc [(k,e)]
  (<+>) (MkAssoc is) (MkAssoc js) = MkAssoc (is++js) -- Kan beter :) 
  findEntry _ (MkAssoc []) = Nothing
  findEntry k (MkAssoc es) = do
    e <- lookup k es
    return e 

-- 3. Complete the definition of PhoneBook, names, phones and owner
data PhoneBook = MkPhoneBook Entry (Assoc Name) (Assoc PhoneNumber) deriving (Show)


names :: PhoneBook -> Assoc Name
names (MkPhoneBook _ n _) = n

phones :: PhoneBook -> Assoc PhoneNumber
phones (MkPhoneBook _ _ p) = p

owner  :: PhoneBook -> Entry
owner (MkPhoneBook o _ _) = o

  
-- 4. Implement byName and byPhone, emptyBook, addToBook, fromEntries

byName :: Name -> PhoneBook -> Maybe Entry
byName name (MkPhoneBook _ ns _) = findEntry name ns

byPhone :: PhoneNumber -> PhoneBook -> Maybe Entry
byPhone phone (MkPhoneBook _ _ ps) = findEntry phone ps 

emptyBook :: Entry -> PhoneBook
emptyBook (MkEntry n p) = MkPhoneBook (MkEntry n p) (empty) (empty)

addToBook :: Entry -> PhoneBook -> PhoneBook
addToBook (MkEntry name phone) (MkPhoneBook o n p) = MkPhoneBook o (n <+> (singleton name (MkEntry name phone))) (p <+> (singleton phone (MkEntry name phone)))

fromEntries :: Entry -> [Entry] -> PhoneBook
fromEntries (MkEntry name phone) es = addManyToBook es (emptyBook (MkEntry name phone))

addManyToBook :: [Entry] -> PhoneBook -> PhoneBook
addManyToBook [] book = book
addManyToBook (e:es) book = addManyToBook es b
  where b = addToBook e book

-- 5. Implement the callerID function.

data Telephone =
  MkTelephone PhoneNumber (PhoneNumber -> IO ())

number  :: Telephone -> PhoneNumber
number (MkTelephone pn _) = pn

receive :: Telephone -> PhoneNumber -> IO ()
receive (MkTelephone _ r) = r

callerID :: PhoneBook -> Telephone
callerID (MkPhoneBook (MkEntry n p) ns ps) = case findEntry p ps of
  Just (MkEntry name phone) -> MkTelephone phone (\p -> print p >> print "Ring ring!")
  Nothing -> case findEntry n ns of
    Just (MkEntry name phone) -> MkTelephone phone (\p -> print p >> print "Ring ring!")
  

-- 6. Calling someone

call :: PhoneBook -> [Telephone] -> IO ()
call = do undefined

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
  -- complete this instance
