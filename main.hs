{-# OPTIONS_GHC -Wall #-}

import Html
import System.Process (readProcess)

-- evil jank
main :: IO ()
main = do
  formatted <- readProcess "tidy" ["-indent", "-quiet"] (show page)
  writeFile "build/index.html" formatted

page :: Html
page = html_ "ur mom" (h_ 1 (escape "Hello, World!"))