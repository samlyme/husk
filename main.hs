{-# OPTIONS_GHC -Wall #-}

import Html
import System.Process (readProcess)

-- evil jank
main :: IO ()
main = do
  formatted <- readProcess "tidy" ["-indent", "-quiet"] (show page)
  writeFile "build/index.html" formatted

page :: Html
page =
  html_
    "This is your title"
    ( h_ 1 (escape "This is a h1")
        <> h_ 2 (escape "This is a h2")
        <> h_ 3 (escape "This is a h3")
        <> h_ 4 (escape "This is a h4")
        <> h_ 5 (escape "This is a h5")
        <> h_ 6 (escape "This is a h6")
        <> p_ (escape "This is a paragraph.")
        <> p_
          ( escape "This is another paragraph."
              <> br_
              <> escape "With a line break."
          )
        <> p_
          ( escape "How about some "
              <> strong_ (escape "bold? ")
              <> escape "Or perhaps some "
              <> em_ (escape "italics? ")
              <> bi_ (escape "Both? ")
          )
        <> p_
          ( escape "Maybe some inline "
              <> code_ (escape "code")
              <> escape "? "
          )
        <> codeBlock_ "python" (escape "# Python more your style?\nprint('Hello, World!')")
        <> quote_
          ( p_ (escape "\"Haskell is the world's finest imperative language.\"")
              <> p_ (em_ (escape "— Simon Peyton Jones"))
          )
        <> hr_
        <> p_ (escape "Pros of Haskell: ")
        <> ol_
          [ li_ (p_ (escape "Pure Functional Programming")),
            li_ (p_ (escape "Strong, Static Type System")),
            li_ (p_ (escape "Excellent Abstractions")),
            li_ (p_ (escape "Great for Compilers and DSLs")),
            li_ (p_ (escape "Lazy Evaluation")),
            li_ (p_ (escape "High Performance (when tuned)")),
            li_ (p_ (escape "Rich Ecosystem for Academic and Research-Grade Tools"))
          ]
        <> p_ (escape "Cons of Haskell: ")
        <> ul_
          [ li_ (p_ (escape "Steep Learning Curve")),
            li_ (p_ (escape "Laziness Can Be Tricky")),
            li_ (p_ (escape "Smaller Ecosystem")),
            li_ (p_ (escape "Cryptic Error Messages")),
            li_ (p_ (escape "Dependency Hell in Some Ecosystems")),
            li_ (p_ (escape "Smaller Community and Hiring Pool"))
          ]
    )