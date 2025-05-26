{-# OPTIONS_GHC -Wall #-}

module Parser.Internal where

import GHC.Natural (Natural)
import Html.Internal (Html, bi_, br_, code_, em_, escape, h_, html_, p_, strong_)

type Markdown = [Block]

data Block
  = Heading Natural [Inline]
  | Paragraph [Inline]
  | OrderedList [Block]
  | UnorderedList [Block]
  | ListItem [Block]
  | CodeBlock String
  | QuoteBlock [Block]
  | HorizontalRule
  deriving (Show, Eq)

data Inline
  = Italic String
  | Bold String
  | ItalicBold String
  | Code String
  | Plain String
  | LineBreak
  deriving (Show, Eq)

render :: Markdown -> Html
render m = html_ "test" (p_ (escape "test"))

renderBlock :: Block -> Html
renderBlock block = case block of
  (Heading n a) -> h_ n (renderLine a)

renderLine :: [Inline] -> Html
renderLine (i : rest) = renderInline i <> renderLine rest
renderLine [] = escape ""

renderInline :: Inline -> Html
renderInline s = case s of
  (Italic s) -> em_ (escape s)
  (Bold s) -> strong_ (escape s)
  (ItalicBold s) -> bi_ (escape s)
  (Code s) -> code_ (escape s)
  (Plain s) -> escape s
  LineBreak -> br_

main :: IO ()
main = do
  -- let h1 = parseHeading 1 "Heading 1"
  -- let h2 = parseHeading 1 "# Heading 2"
  -- mapM_ print [h1, h2]

  raw <- readFile "content/test.md"
  let parsed = parse raw
  mapM_ print parsed

parse :: String -> Markdown
parse s = parseLines Nothing (lines s)

-- Takes in the current block you are working on, and the rest of the strings
parseLines :: Maybe Block -> [String] -> Markdown
parseLines Nothing [] = []
parseLines (Just a) [] = [a]
parseLines Nothing (currentLine : sl) =
  if trim currentLine == ""
    then parseLines Nothing sl
    else case parseLine currentLine of
      (Paragraph p) -> parseLines (Just (Paragraph p)) sl
      (OrderedList o) -> parseLines (Just (OrderedList o)) sl
      (UnorderedList u) -> parseLines (Just (UnorderedList u)) sl
      (CodeBlock c) -> parseLines (Just (CodeBlock c)) sl
      (QuoteBlock q) -> parseLines (Just (QuoteBlock q)) sl
      b -> parseLines (Just b) sl
parseLines (Just (Paragraph a)) (currentLine : sl) =
  if trim currentLine == ""
    then Paragraph a : parseLines Nothing sl
    else case parseLine currentLine of
      (Paragraph p) -> parseLines (Just (Paragraph (a ++ [LineBreak] ++ p))) sl
      b -> Paragraph a : parseLines (Just b) sl
parseLines (Just a) ls = a : parseLines Nothing ls

parseLine :: String -> Block
parseLine s =
  case s of
    ('#' : rest) -> parseHeading 1 rest
    _ -> parseParagraph s

parseHeading :: Natural -> String -> Block
parseHeading n ('#' : s) = parseHeading (n + 1) s
parseHeading n s = Heading n (parseInline (trim s))

parseParagraph :: String -> Block
parseParagraph s = Paragraph (parseInline s)

trim :: String -> String
trim = unwords . words

-- Entry point
parseInline :: String -> [Inline]
parseInline "" = []
parseInline s
  | "***" `isPrefixOf` s =
      let (content, rest) = extractBetweenEsc "***" (drop 3 s)
       in ItalicBold content : parseInline rest
  | "**" `isPrefixOf` s =
      let (content, rest) = extractBetweenEsc "**" (drop 2 s)
       in Bold content : parseInline rest
  | "*" `isPrefixOf` s =
      let (content, rest) = extractBetweenEsc "*" (drop 1 s)
       in Italic content : parseInline rest
  | "`" `isPrefixOf` s =
      let (content, rest) = extractBetweenEsc "`" (drop 1 s)
       in Code content : parseInline rest
  | otherwise =
      let (plainText, rest) = spanUntilSpecialEsc s
       in Plain plainText : parseInline rest

-- Extract text between delimiters with escape handling
extractBetweenEsc :: String -> String -> (String, String)
extractBetweenEsc delim = go ""
  where
    go acc [] = (acc, []) -- Unterminated case
    go acc ('\\' : x : xs) = go (acc ++ [x]) xs -- Escaped character
    go acc s@(c : cs)
      | delim `isPrefixOf` s = (acc, drop (length delim) s)
      | otherwise = go (acc ++ [c]) cs

-- Span plain text until unescaped formatting character
spanUntilSpecialEsc :: String -> (String, String)
spanUntilSpecialEsc = go ""
  where
    go acc [] = (acc, [])
    go acc ('\\' : x : xs) = go (acc ++ [x]) xs -- Escaped character
    go acc s@(c : _)
      | isFormatStart s = (acc, s)
      | otherwise = go (acc ++ [c]) (tail s)

-- Detect if the next unescaped characters begin a format marker
isFormatStart :: String -> Bool
isFormatStart s =
  any (`isPrefixOf` s) ["***", "**", "*", "`"]

isPrefixOf :: String -> String -> Bool
isPrefixOf p s = take (length p) s == p