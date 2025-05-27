{-# OPTIONS_GHC -Wall #-}

module Parser.Internal where

import Data.Char (isDigit)
import GHC.Natural (Natural)

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

data Inline
  = Plain String
  | Italic [Inline]
  | Bold [Inline]
  | LineBreak

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
parseLines (Just (CodeBlock l c)) (currentLine : rest) =
  case parseLine currentLine of
    (CodeBlock _ _) -> let (m, s) = parseLines Nothing rest in (CodeBlock l c : m, s)
    _ -> parseLines (Just (CodeBlock l (currentLine : c))) rest
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
      ('`' : '`' : '`' : rest) -> CodeBlock rest []
      ('-' : '-' : '-' : _) -> HorizontalRule
      (' ' : rest) -> parseIndented 1 rest
      ('-' : rest) ->
        case parseUnorderedList 0 rest of
          Just a -> a
          Nothing -> Text (fst (parseInline (Plain "") s))
      ('>' : rest) -> parseQuotes 1 (trim rest)
      ('#' : rest) -> parseHeading 1 rest
      (c : rest) ->
        if isDigit c
          then case parseOrderedList 0 rest of
            Just a -> a
            Nothing -> parseParagraph s
          else parseParagraph s
      _ -> parseParagraph s

parseInline :: Inline -> String -> ([Inline], String)
parseInline a "" = ([a], "")
parseInline LineBreak rest =
  let (m, r) = parseInline (Plain "") rest
   in (LineBreak : m, r)
-- Currently Plain
parseInline (Plain p) ('*' : '*' : rest) =
  let (m, r) = parseInline (Bold []) rest
   in (Plain p : m, r)
parseInline (Plain p) ('*' : rest) =
  let (m, r) = parseInline (Italic []) rest
   in (Plain p : m, r)
parseInline (Plain p) (c : rest) = parseInline (Plain (c : p)) rest
-- Currently Bold
parseInline (Bold b) ('*' : '*' : rest) =
  let (m, r) = parseInline (Plain "") rest
   in (Bold b : m, r)
parseInline (Bold b) ('*' : rest) =
  let (m, r) = parseInline (Italic []) rest
   in ([Bold (b ++ m)], r)
parseInline (Bold b) (c : rest) =
  case b of
    [] -> let nb = Bold [Plain [c]] in parseInline nb rest
    (Plain p : r) -> let nb = Bold (Plain (c : p) : r) in parseInline nb rest
    r -> let nb = Bold (Plain [c] : r) in parseInline nb rest
-- Currently Italic
parseInline (Italic i) ('*' : '*' : rest) =
  let (m, r) = parseInline (Bold []) rest
   in ([Italic (i ++ m)], r)
parseInline (Italic i) ('*' : rest) =
  let (m, r) = parseInline (Plain "") rest
   in (Italic i : m, r)
parseInline (Italic i) (c : rest) =
  case i of
    [] -> let ni = Italic [Plain [c]] in parseInline ni rest
    (Plain p : r) -> let ni = Italic (Plain (c : p) : r) in parseInline ni rest
    r -> let ni = Italic (Plain [c] : r) in parseInline ni rest

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