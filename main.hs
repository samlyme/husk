{-# OPTIONS_GHC -Wall #-}

import Html
import Parser (parse)
import Renderer (render)
import System.Process (readProcess)

-- evil jank
main :: IO ()
main = do
  raw <- readFile "content/index.md"
  let ast = parse raw
  let page = html_ "my title" (render ast)
  -- let page1 = html_ "render test" (render testAst)
  formatted <- readProcess "tidy" ["-indent", "-quiet"] (show page)
  -- formatted1 <- readProcess "tidy" ["-indent", "-quiet"] (show page1)
  writeFile "build/index.html" formatted