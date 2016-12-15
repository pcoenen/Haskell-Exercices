module PhonebookTests where

import Phonebook
import Control.Monad
import Data.Maybe (isNothing, isJust)
import Prelude hiding (catch, foldr1, foldl1) -- Required for older versions of GHC
import Data.Monoid
import Control.Exception (catch, SomeException(..))

-- Some tests
main :: IO ()
main = startTests
       -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Entry~~~~~~~~~~~~~~~~~~~~~~~~~~
       -- Create an Entry and test name
       <> test "(name (mkEntry \"john\" [911]) == \"john\")"
           (name (mkEntry "john" [911]) == "john")
       -- Create an Entry and test phone
       <> test "(name (mkEntry \"john\" [911]) == [911])"
           (phone (mkEntry "john" [911]) == [911])
       -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Index ~~~~~~~~~~~~~~~~~~~~~~~~~~~
       -- Create an empty Assoc
       <> test "(findEntry \"Bill\" (empty :: Assoc ()) == Nothing)"
           (findEntry "Bill" (empty :: Assoc Name) == Nothing)
       -- Create a singleton Assoc
       <> test "(findEntry \"Bill\" (singleton \"Bill\" bill) == Just bill)"
           (findEntry "Bill" (singleton "Bill" bill :: Assoc Name) == Just bill)
       -- Combine two Assocs
       <> test "(findEntry \"Bill\" (singleton \"Bill\" bill <> singleton \"Bob\" bob) == Just bill)"
           (findEntry "Bill" (singleton "Bill" bill<+> singleton "Bob" bob :: Assoc Name) == Just bill)
       <> test "(findEntry \"Bob\" (singleton \"Bill\" bill <> singleton \"Bob\" bob) == Just bob)"
           (findEntry "Bob" (singleton "Bill" bill <+> singleton "Bob" bob :: Assoc Name) == Just bob)
       <> test "(findEntry [0] (singleton [0] bill <+> singleton [0] bob) == Just bill)"
           (findEntry [0] (singleton [0] bill <+> singleton [0] bob :: Assoc PhoneNumber) == Just bill)
       -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Phonebook ~~~~~~~~~~~~~~~~~~~~~~~~~~~
       -- Create an empty Phonebook test with byName and byPhone
       <> test "(owner (emptyBook bill) == Just bill)"
           (owner (emptyBook bill) == bill)
       <> test "(byName \"Bill\" (emptyBook bill) == Nothing)"
           (byName "Bill" (emptyBook bill) == Nothing)
       <> test "(byPhone [32,444,123] (emptyBook bill) == Nothing)"
           (byPhone [32,444,123] (emptyBook bill) == Nothing)
       -- Add an Entry to a Phonebook
       <> test "(byName \"Bob\" (addToBook bob (emptyBook bill) == Just bob)"
           (byName "Bob" (addToBook bob (emptyBook bill)) == Just bob)
       <> test "(byPhone [32,444,123] (addToBook bob (emptyBook bill) == Just bob)"
           (byPhone [32,444,124] (addToBook bob (emptyBook bill)) == Just bob)
       -- create a Phonebook from al ist of entries
       <> test "(byName \"Jebediah\" bobbook == Just jeb)"
            (byName "Jebediah" bobbook == Just jeb)
       <> test "(byName \"Bill\" bobbook == Just bill)"
            (byName "Bill" bobbook == Just bill)
       <> test "(byName \"Valentina\" bobbook == Nothing)"
            (byName "Valentina" bobbook == Nothing)
       <> test "(byName \"Valentina\" jebbook == Just val)"
            (byName "Valentina" jebbook == Just val)
       <> test "(byPhone [32,444,123] valbook == Just bill)"
            (byPhone [32,444,123] valbook == Just bill)
       <> test "(byPhone [32,444,124] valbook == Just bob)"
            (byPhone [32,444,124] valbook == Just bob)
       <> test "(byPhone [32,444,125] valbook == Just jeb)"
            (byPhone [32,444,125] valbook == Just jeb)
       <> test "(byPhone [32,444,126] valbook == Nothing)"
            (byPhone [32,444,126] valbook == Nothing)
       <> test "(number (callerID billbook) == 32 444 123)"
            (number (callerID billbook) == phone bill)
       -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Lookup ~~~~~~~~~~~~~~~~~~~~~~~~~~~
       -- Create an empty Lookup
       <> test "(findEntry \"Bill\" (empty :: Lookup Name) == Nothing)"
           (findEntry "Bill" (empty :: Lookup Name) == Nothing)
       -- Create a singleton Assoc
       <> test "(findEntry \"Bill\" (singleton \"Bill\" bill :: Lookup Name) == Just bill)"
           (findEntry "Bill" (singleton "Bill" bill :: Lookup Name) == Just bill)
       -- Combine two Assocs
       <> test "(findEntry \"Bill\" (singleton \"Bill\" bill <> singleton \"Bob\" bob :: Lookup Name) == Just bill)"
           (findEntry "Bill" (singleton "Bill" bill<+> singleton "Bob" bob :: Lookup Name) == Just bill)
       <> test "(findEntry \"Bob\" (singleton \"Bill\" bill <> singleton \"Bob\" bob) == Just bob)"
           (findEntry "Bob" (singleton "Bill" bill <+> singleton "Bob" bob :: Lookup Name) == Just bob)
       <> test "(findEntry [0] (singleton [0] bill <+> singleton [0] bob :: Lookup PhoneNumber) == Just bill)"
           (findEntry [0] (singleton [0] bill <+> singleton [0] bob :: Lookup PhoneNumber) == Just bill)

       >>= endTests

-- Mini testing framework
test :: String -> Bool -> IO Results
test msg b
  = do notImplemented <- isUndefined b
       case notImplemented of
         True      -> printResult yellow "function not implemented" >> return (Sum 1, Sum 0, Sum 0)
         False | b -> printResult green "passed" >> return (Sum 0, Sum 0, Sum 1)
         _         -> printResult red "failed" >> return (Sum 0, Sum 1, Sum 0)
  where printResult colorCode result
          = putStrLn $ "Test " ++ msg ++ " " ++ colorise colorCode result

type Results = (Sum Int, Sum Int, Sum Int) -- (Not implemented, failed, passed)

--instance Monoid a => Monoid (IO a) where
--  mempty = return mempty
--  mappend = liftM2 mappend

startTests :: IO Results
startTests = putStrLn "Testing your solutions" >> return (Sum 0, Sum 0, Sum 0)

endTests :: Results -> IO ()
endTests (notImpl, failed, passed)
  = case (getSum notImpl, getSum failed, getSum passed) of
     (0, 0, _) -> putStrLn $ colorise green "All tests passed"
     (n, f, p) -> putStrLn $ unwords $
                  filter (not . null) [nNotImpl n, nFailed f, nPassed p]
  where nPassed 0 = ""
        nPassed p = colorise green $ show p ++ " tests passed"
        nFailed 0 = ""
        nFailed f = colorise red $ show f ++ " tests failed"
        nNotImpl 0 = ""
        nNotImpl n = colorise yellow $ show n ++ "x function not implemented"

isUndefined :: a -> IO Bool
isUndefined a = (a `seq` return False) `catch` \(SomeException _) -> return True

red, green, yellow :: Int
(red, green, yellow) = (31, 32, 33)

colorise :: Int -> String -> String
colorise colorCode s = "\ESC[0;" ++ show colorCode ++ "m" ++ s ++ "\ESC[0m"
