{-# OPTIONS_GHC -Wall #-}

module Html.Internal where

import GHC.Num (Natural)

-- This data type exists because there are times where
-- we can pass either an html structure or an escaped string.
-- eg. <p> <em> bold </em> </p>
data Html = Elements Structure | Text String

newtype Structure = Structure String

newtype Attribute = Attribute String

type Title = String

-- This function is just cursed.
-- It is meant to create an html template
html_ :: String -> Structure -> Html
html_ title content =
  Text
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

h_ :: Natural -> Html -> Structure
h_ n = Structure . el ("h" <> show n) . escape

p_ :: String -> Structure
p_ = Structure . el "p" . escape

br_ :: Structure
br_ = Structure "<br>"

strong_ :: String -> Structure
strong_ = Structure . el "strong" . escape

em_ :: String -> Structure
em_ = Structure . el "em" . escape

quote_ :: Structure -> Structure
quote_ = Structure . div_ "quote"

ol_ :: [Structure] -> Structure
ol_ =
  let li_ = el "li"
   in Structure . el "ol" . concatMap (li_ . getStructureString)

ul_ :: [Structure] -> Structure
ul_ =
  let li_ = el "li"
   in Structure . el "ul" . concatMap (li_ . getStructureString)

code_ :: [Char] -> Structure
code_ = Structure . el "code" . escape

-- img_ :: String -> String -> Structure

-- internal
div_ :: String -> Structure -> String
div_ c = ela "div" [attr "class" c]

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

ela :: String -> [Attribute] -> Structure -> String
ela tag attrs content =
  case attrs of
    [] -> el tag (getStructureString content)
    _ ->
      "<"
        <> tag
        <> " "
        <> unwords (map getAttributeString attrs)
        <> ">"
        <> getStructureString content
        <> "</"
        <> tag
        <> ">"

getAttributeString :: Attribute -> String
getAttributeString (Attribute s) = s

attr :: String -> String -> Attribute
attr a s = Attribute (a <> "=\"" <> s <> "\"")

-- If you squint, (Structure str) is how we create a variable of type Structure
-- Since everything is functional, having this definition in the args
-- gives us access to the underlying data.
getStructureString :: Structure -> String
getStructureString (Structure str) = str

-- I feel like escaping should not be the responsibility of
--  the html library
escape :: [Char] -> [Char]
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar

instance Semigroup Structure where
  (<>) :: Structure -> Structure -> Structure
  (<>) a b =
    Structure (getStructureString a <> getStructureString b)

render :: Html -> String
render a =
  case a of
    (Text t) -> t
    (Elements e) -> getStructureString e