
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
  toHtml (Link x y) = HtmlTag "a" [Attr "href" x] [HtmlString y]

instance HTML a => HTML [a] where
  toHtml x  = HtmlTag "ul" [] [toHtml a | a <- x]

-- The encoding of the following unordered list as an HtmlElement
--   <ul>
--   <li>Appels</li>
--   <li>Bananas</li>
--   <li>Oranges</li>
--   </ul>
exampleUL :: HtmlElement
exampleUL = HtmlTag "ul" [] [HtmlTag "li" [] [HtmlString "Appels"],HtmlTag "li" [] [HtmlString "Bananas"], HtmlTag "li" [] [HtmlString "Oranges"]]

data Mail = Private String 
    | Work String
    deriving (Eq, Show)

data Adress = Ad String String [Mail]
    deriving (Eq, Show)

data AddressBook = Book [Adress]
    deriving (Eq, Show)

myAddressBook :: AddressBook
myAddressBook = Book [Ad "Jan" "Claes" [Private "jan@private.fic", Work "jan@work.fic"], Ad "Lien" "Bosmans" [Private "lien@home.fic", Work "lien@job.fic"]]

instance HTML AddressBook where
  toHtml x = error "Not implemented"

renderElement :: HtmlElement -> String
renderElement = error "Not implemented"

render :: HTML a => a -> String
render = renderElement . toHtml


