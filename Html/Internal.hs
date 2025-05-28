{-# OPTIONS_GHC -Wall #-}

module Html.Internal where

import GHC.Natural (Natural)

-- This data type exists because there are times where
-- we can pass either an html structure or an escaped string.
-- eg. <p> <em> bold </em> </p>
newtype Html = Html String

instance Show Html where
  show :: Html -> String
  show (Html s) = s

instance Semigroup Html where
  (<>) :: Html -> Html -> Html
  (<>) a b = Html (show a <> show b)

type EscapedString = Html

newtype Attribute = Attribute String

-- ultra cursed
html_ :: String -> Html -> Html
html_ title content =
  Html (ot "!DOCTYPE html")
    <> ela
      "html"
      [attr "lang" "en"]
      ( el
          "head"
          ( el "title" (escape title)
              <> iela "meta" [attr "charset" "UTF-8"]
              <> iela "meta" [attr "name" "viewport", attr "content" "width=device-width, initial-scale=1.0"]
              -- make an option to change light/dark theme and the code syntax theme
              <> iela "link" [attr "rel" "stylesheet", attr "href" "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/atom-one-dark.min.css"]
              <> iela "link" [attr "rel" "stylesheet", attr "href" "./style-light.css"]
              <> ela "script" [attr "src" "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js"] (escape "")
              <> el "script" (Html "hljs.highlightAll();")
          )
          <> el "body" (el "main" content)
      )

h_ :: Natural -> Html -> Html
h_ n = el ("h" <> show n)

p_ :: Html -> Html
p_ = el "p"

br_ :: Html
br_ = iel "br"

hr_ :: Html
hr_ = iel "hr"

strong_ :: Html -> Html
strong_ = el "strong"

em_ :: Html -> Html
em_ = el "em"

-- bold italic
bi_ :: Html -> Html
bi_ = strong_ . em_

code_ :: Html -> Html
code_ = el "code"

-- These will be highlighted
codeBlock_ :: String -> EscapedString -> Html
codeBlock_ lang content =
  el "pre" (ela "code" [attr "class" ("language-" <> lang)] content)

quote_ :: Html -> Html
quote_ = el "blockquote"

ol_ :: Html -> Html
ol_ = el "ol"

ul_ :: Html -> Html
ul_ = el "ul"

li_ :: Html -> Html
li_ = el "li"

a_ :: Html -> String -> Html
a_ title ref = ela "a" [attr "href" ref] title

img_ :: String -> String -> Html
img_ alt src = iela "img" [attr "alt" alt, attr "src" src]

el :: String -> Html -> Html
el tag content =
  Html (ot tag <> show content <> ct tag)

ela :: String -> [Attribute] -> Html -> Html
ela tag attributes content =
  Html
    ( ota tag attributes
        <> show content
        <> ct tag
    )

-- Inline element
iel :: String -> Html
iel tag = Html ("<" <> tag <> " />")

-- Inline element with attributes. Maybe fix with monads or whatever in future
iela :: String -> [Attribute] -> Html
iela tag attributes =
  Html ("<" <> tag <> getAttributesString attributes <> " />")

ota :: String -> [Attribute] -> String
ota tag attributes = "<" <> tag <> getAttributesString attributes <> ">"

ot :: String -> String
ot tag = "<" <> tag <> ">"

ct :: String -> String
ct tag = "</" <> tag <> ">"

attr :: String -> String -> Attribute
attr a s = Attribute (a <> "=\"" <> s <> "\"")

concatHtml :: [Html] -> Html
concatHtml = Html . concatMap show

getAttributesString :: [Attribute] -> String
getAttributesString attributes = ' ' : unwords (map (\(Attribute x) -> x) attributes)

-- This is the only way to create an EscapedString
escape :: String -> Html
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in Html . concatMap escapeChar

type Title = String