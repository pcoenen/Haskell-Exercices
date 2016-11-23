
module Template where

-- * HTML
-- ----------------------------------------------------------------------------

-- Simple (X)HTML markup.
data Attr = Attr String String
  deriving (Eq,Show)

data HtmlElement
  = HtmlString String                    -- Plain text.
  | HtmlTag String [Attr] HtmlElements   -- Structured markup.
  deriving (Eq, Show)

type HtmlElements = [HtmlElement]

example :: HtmlElement
example =
  HtmlTag "a" [Attr "href" "https://www.kuleuven.be/kuleuven/"]
    [HtmlString "KU Leuven"]

-- HTML renderable class.
class HTML a where
  toHtml :: a -> HtmlElement

data Link =
  Link
    String  -- Link target.
    String  -- Text to show.
  deriving (Eq,Show)

instance HTML Link where
  toHtml (Link link text) = HtmlTag "a" [Attr "href" link] [HtmlString text]


-- Input is a list of elements from the HTML class
instance HTML a => HTML [a] where
  toHtml list = HtmlTag "ul" [] (map (\x -> HtmlTag "li" [] [toHtml x]) list)
 
-- The encoding of the following unordered list as an HtmlElement
--   <ul>
--   <li>Appels</li>
--   <li>Bananas</li>
--   <li>Oranges</li>
--   </ul>

exampleUL :: HtmlElement
exampleUL = HtmlTag "ul" [] [HtmlTag "li" [] [HtmlString "Appels"], HtmlTag "li" [] [HtmlString "Bananas"], HtmlTag "li" [] [HtmlString "Oranges"]]

data Adress =
    Adress
    String -- Firstname
    String -- Lastname
    [Email]

data Email = Work String | Private String

data AddressBook = AdressBook [Adress]
    

myAddressBook :: AddressBook
myAddressBook = AdressBook [Adress "Pieter-Jan" "Coenen" [Private "pieterjan.coenen@gmail.com", Work "pieterjan.student@kuleuven.be"], Adress "Yana" "Dimova" [Work "yanateeuh@jupiler.be"]]

instance HTML AddressBook where
  toHtml (AdressBook values) = toHtml values

-- Only one email adress will be shown
instance HTML Adress where
  toHtml (Adress fn ln ((Work x):xs)) = HtmlTag "a" [Attr "href" ("mailto:" ++ x)] [HtmlString (fn ++ " " ++ ln)]
  toHtml (Adress fn ln ((Private x):xs)) = HtmlTag "a" [Attr "href" ("mailto:" ++ x)] [HtmlString (fn ++ " " ++ ln)]

