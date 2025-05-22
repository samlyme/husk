{-# OPTIONS_GHC -Wall #-}

module Html.Internal where

-- This data type exists because there are times where
-- we can pass either an html structure or an escaped string.
-- eg. <p> <em> bold </em> </p>
data Html = Structure Element | Text EscapedString

newtype Element = Element String

newtype EscapedString = EscapedString String

newtype Attribute = Attribute String

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

ota :: String -> [Attribute] -> String
ota tag attributes = "<" <> tag <> unwords (map (\(Attribute x) -> x) attributes) <> ">"

ot :: String -> String
ot tag = "<" <> tag <> ">"

ct :: String -> String
ct tag = "</" <> tag <> ">"

attr :: String -> String -> Attribute
attr a s = Attribute (a <> "=\"" <> s <> "\"")

getHtmlString :: Html -> String
getHtmlString content = case content of
  (Text s) -> (\(EscapedString e) -> e) s
  (Structure s) -> (\(Element e) -> e) s

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