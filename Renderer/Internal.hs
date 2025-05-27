module Renderer.Internal where

import Html
import Parser (Block (..), Inline (..), Markdown)

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