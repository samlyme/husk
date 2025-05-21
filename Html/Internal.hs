{-# OPTIONS_GHC -Wall #-}

module Html.Internal where

import GHC.Num (Natural)

newtype Html = Html String

newtype Structure = Structure String

type Title = String

html_ :: String -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

h_ :: Natural -> String -> Structure
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

-- internal
div_ :: String -> Structure -> String
div_ c (Structure content) = "<div class=\"" <> c <> "\">" <> content <> "</div>"

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- If you squint, (Structure str) is how we create a variable of type Structure
-- Since everything is functional, having this definition in the args
-- gives us access to the underlying data.
getStructureString :: Structure -> String
getStructureString (Structure str) = str

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
render (Html a) = a