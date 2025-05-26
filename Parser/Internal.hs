{-# OPTIONS_GHC -Wall #-}

module Parser.Internal where

import GHC.Natural (Natural)
import Html.Internal (Html, escape, html_, p_)

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
parseLines currentBlock ls =
  case (currentBlock, ls) of
    (_, []) -> []
    (context, currentLine : rest) ->
      let pcl = parseLine context currentLine
       in if trim currentLine == ""
            then
              parseLines Nothing rest
            else
              pcl : parseLines (Just pcl) rest -- Wrap in paragraph for now

parseLine :: Maybe Block -> String -> Block
parseLine context s =
  case s of
    ('#' : rest) -> parseHeading 1 rest
    _ -> parseParagraph context s

parseHeading :: Natural -> String -> Block
parseHeading n ('#' : s) = parseHeading (n + 1) s
parseHeading n s = Heading n (parseInline (trim s))

parseParagraph :: Maybe Block -> String -> Block
parseParagraph context s =
  case context of
    (Just (Paragraph l)) -> Paragraph (l ++ parseInline s)
    _ -> Paragraph (parseInline s)

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

render :: Markdown -> Html
render m = html_ "test" (p_ (escape "test"))