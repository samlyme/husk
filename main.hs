{-# OPTIONS_GHC -Wall #-}

import Husk.Html
import Husk.Parser (parse)
import Husk.Renderer (render)

main :: IO ()
main = do
  raw <- readFile "content/index.md"
  let ast = parse raw
  let page = html_ "my title" (render ast)
  writeFile "build/index.html" (show page)