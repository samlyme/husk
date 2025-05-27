{-# OPTIONS_GHC -Wall #-}

module Parser.Internal where

import Data.Char (isDigit)
import GHC.Natural (Natural)
import Html.Internal (Html, bi_, br_, codeBlock_, code_, concatHtml, em_, escape, h_, hr_, li_, ol_, p_, quote_, strong_, ul_)

type Markdown = [Block]

data Block
  = Heading Natural [Inline]
  | Paragraph [Inline]
  | OrderedList Natural [Block]
  | UnorderedList Natural [Block]
  | ListItem [Block]
  | Text [Inline]
  | Indented [Inline]
  | CodeBlock String [String]
  | QuoteBlock Natural [Block]
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
render = foldr ((<>) . renderBlock) (escape "")

renderBlock :: Block -> Html
renderBlock block = case block of
  (Heading n a) -> h_ n (renderLine a)
  (Paragraph a) -> p_ (renderLine a)
  (OrderedList _ a) -> ol_ (concatHtml (map renderBlock (reverse a)))
  (UnorderedList _ a) -> ul_ (concatHtml (map renderBlock (reverse a)))
  (ListItem a) -> li_ (concatHtml (map renderBlock a))
  (Text t) -> renderLine t
  (Indented i) -> p_ (renderLine i)
  (CodeBlock l a) -> codeBlock_ l (escape (unlines (reverse a)))
  (QuoteBlock _ a) -> quote_ (concatHtml (map renderBlock a))
  HorizontalRule -> hr_

renderLine :: [Inline] -> Html
renderLine = foldr ((<>) . renderInline) (escape "\n")

renderInline :: Inline -> Html
renderInline i = case i of
  (Italic s) -> em_ (escape s)
  (Bold s) -> strong_ (escape s)
  (ItalicBold s) -> bi_ (escape s)
  (Code s) -> code_ (escape s)
  (Plain s) -> escape s
  LineBreak -> br_

parse :: String -> Markdown
parse s = fst (parseLines Nothing (lines s))

-- Takes in the current block you are working on, and the rest of the strings
parseLines :: Maybe Block -> [String] -> (Markdown, [String])
parseLines Nothing [] = ([], [])
parseLines (Just a) [] = ([a], [])
parseLines Nothing (currentLine : rest) =
  if trim currentLine == ""
    then let (m, s) = parseLines Nothing rest in (m, s)
    else case parseLine currentLine of
      (Paragraph p) -> parseLines (Just (Paragraph p)) rest
      b -> let (m, s) = parseLines (Just b) rest in (m, s)
parseLines (Just (Paragraph p)) (currentLine : rest) =
  if trim currentLine == ""
    then let (m, s) = parseLines Nothing rest in (Paragraph p : m, s)
    else case parseLine currentLine of
      (Paragraph l) -> parseLines (Just (Paragraph (p ++ [LineBreak] ++ l))) rest
      b -> let (m, s) = parseLines (Just b) rest in (Paragraph p : m, s)
parseLines (Just (UnorderedList d u)) (currentLine : rest) =
  if trim currentLine == ""
    then let (m, s) = parseLines Nothing rest in (UnorderedList d u : m, s)
    else case parseLine currentLine of
      (UnorderedList d1 u1) ->
        if d == d1
          then parseLines (Just (UnorderedList d (u1 ++ u))) rest
          else
            if d < d1
              then
                let (m, s) = parseLines (Just (UnorderedList d1 u1)) rest
                 in case u of
                      (ListItem li : r) -> parseLines (Just (UnorderedList d (ListItem (li ++ m) : r))) s
                      _ -> parseLines (Just (UnorderedList d (m ++ u))) s
              else ([UnorderedList d u], currentLine : rest)
      b -> let (m, s) = parseLines (Just b) rest in (UnorderedList d m : u, s)
parseLines (Just (OrderedList d u)) (currentLine : rest) =
  if trim currentLine == ""
    then let (m, s) = parseLines Nothing rest in (OrderedList d u : m, s)
    else case parseLine currentLine of
      (OrderedList d1 u1) ->
        if d == d1
          then parseLines (Just (OrderedList d (u1 ++ u))) rest
          else
            if d < d1
              then
                let (m, s) = parseLines (Just (OrderedList d1 u1)) rest
                 in case u of
                      (ListItem li : r) -> parseLines (Just (OrderedList d (ListItem (li ++ m) : r))) s
                      _ -> parseLines (Just (OrderedList d (m ++ u))) s
              else ([OrderedList d u], currentLine : rest)
      b -> let (m, s) = parseLines (Just b) rest in (OrderedList d m : u, s)
parseLines (Just (QuoteBlock d q)) (currentLine : rest) =
  if trim currentLine == ""
    then let (m, s) = parseLines Nothing rest in (QuoteBlock d q : m, s)
    else case parseLine currentLine of
      (QuoteBlock d1 q1) ->
        if d == d1
          then parseLines (Just (QuoteBlock d (q ++ q1))) rest
          else
            if d < d1
              then
                let (m, s) = parseLines (Just (QuoteBlock d1 q1)) rest
                 in parseLines (Just (QuoteBlock d (q ++ m))) s
              else ([QuoteBlock d q], currentLine : rest)
      b -> let (m, s) = parseLines (Just b) rest in (QuoteBlock d q : m, s)
parseLines (Just a) ls =
  let (m, s) = parseLines Nothing ls
   in (a : m, s)

parseLine :: String -> Block
parseLine s =
  case matchPrefixN 3 '`' s of
    (Just l) -> CodeBlock l []
    _ -> case s of
      (' ' : rest) -> parseIndented 1 rest
      ('-' : rest) ->
        case parseUnorderedList 0 rest of
          Just a -> a
          Nothing -> Text (parseInline s)
      ('>' : rest) -> parseQuotes 1 (trim rest)
      ('#' : rest) -> parseHeading 1 rest
      (c : rest) ->
        if isDigit c
          then case parseOrderedList 0 rest of
            Just a -> a
            Nothing -> parseParagraph s
          else parseParagraph s
      _ -> parseParagraph s

parseIndented :: Natural -> String -> Block
parseIndented d (' ' : s) = parseIndented (d + 1) s
parseIndented d ('-' : s) =
  case parseUnorderedList d s of
    Just a -> a
    Nothing -> Text (parseInline s)
parseIndented d (c : rest) =
  if isDigit c
    then case parseOrderedList d rest of
      Just a -> a
      Nothing -> parseParagraph (c : rest)
    else parseParagraph (c : rest)
parseIndented _ s = Indented (parseInline s)

parseUnorderedList :: Natural -> String -> Maybe Block
parseUnorderedList d (' ' : s) = Just (UnorderedList d [ListItem [Text (parseInline s)]])
parseUnorderedList _ _ = Nothing

parseOrderedList :: Natural -> String -> Maybe Block
parseOrderedList d ('.' : ' ' : s) = Just (OrderedList d [ListItem [Text (parseInline s)]])
parseOrderedList d (c : s) =
  if isDigit c
    then parseOrderedList d s
    else Nothing
parseOrderedList _ _ = Nothing

parseQuotes :: Natural -> String -> Block
parseQuotes d ('>' : s) = parseQuotes (d + 1) (trim s)
parseQuotes d s = QuoteBlock d [parseLine (trim s)]

parseHeading :: Natural -> String -> Block
parseHeading n ('#' : s) = parseHeading (n + 1) s
parseHeading n s = Heading n (parseInline (trim s))

parseParagraph :: String -> Block
parseParagraph s = Paragraph (parseInline s)

trim :: String -> String
trim = unwords . words

matchPrefixN :: Int -> Char -> String -> Maybe String
matchPrefixN n c s =
  let (prefix, rest) = span (== c) s
   in if length prefix == n then Just rest else Nothing

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