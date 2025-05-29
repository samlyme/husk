{-# OPTIONS_GHC -Wall #-}

import Html
import Parser (parse)
import Renderer (render)

main :: IO ()
main = do
  raw <- readFile "content/index.md"
  let ast = parse raw
  let page = html_ "my title" (render ast)
  writeFile "build/index.html" (show page)