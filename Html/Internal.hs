{-# OPTIONS_GHC -Wall #-}

module Html.Internal where

import GHC.Natural (Natural)

-- This data type exists because there are times where
-- we can pass either an html structure or an escaped string.
-- eg. <p> <em> bold </em> </p>
data Html = Structure Element | Text EscapedString

newtype Element = Element String

newtype EscapedString = EscapedString String

newtype Attribute = Attribute String

h_ :: Natural -> Html -> Html
h_ n = el ("h" <> show n)

p_ :: Html -> Html
p_ = el "p"

br_ :: Html
br_ = iel "br"

strong_ :: Html -> Html
strong_ = el "strong"

em_ :: Html -> Html
em_ = el "em"

-- bold italic
bi_ :: Html -> Html
bi_ = strong_ . em_

code_ :: Html -> Html
code_ = el "code"

quote_ :: Html -> Html
quote_ = ela "div" [attr "class" "quote"]

ol_ :: [Html] -> Html
ol_ items = el "ol" (concatHtml (map li_ items))

ul_ :: [Html] -> Html
ul_ items = el "ul" (concatHtml (map li_ items))

li_ :: Html -> Html
li_ = el "li"

el :: String -> Html -> Html
el tag content =
  Structure (Element (ot tag <> getHtmlString content <> ct tag))

ela :: String -> [Attribute] -> Html -> Html
ela tag attributes content =
  Structure
    ( Element
        ( ota tag attributes
            <> getHtmlString content
            <> ct tag
        )
    )

-- Inline element
iel :: String -> Html
iel tag = Structure (Element ("<" <> tag <> " />"))

-- Inline element with attributes. Maybe fix with monads or whatever in future
iela :: String -> [Attribute] -> Html
iela tag attributes =
  Structure (Element ("<" <> tag <> getAttributesString attributes <> " />"))

ota :: String -> [Attribute] -> String
ota tag attributes = "<" <> tag <> getAttributesString attributes <> ">"

ot :: String -> String
ot tag = "<" <> tag <> ">"

ct :: String -> String
ct tag = "</" <> tag <> ">"

attr :: String -> String -> Attribute
attr a s = Attribute (a <> "=\"" <> s <> "\"")

concatHtml :: [Html] -> Html
concatHtml = Structure . Element . concatMap getHtmlString

getHtmlString :: Html -> String
getHtmlString content = case content of
  (Text s) -> (\(EscapedString e) -> e) s
  (Structure s) -> (\(Element e) -> e) s

getAttributesString :: [Attribute] -> String
getAttributesString attributes = ' ' : unwords (map (\(Attribute x) -> x) attributes)

-- This is the only way to create an EscapedString
escape :: String -> EscapedString
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in EscapedString . concatMap escapeChar

type Title = String