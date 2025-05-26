{-# OPTIONS_GHC -Wall #-}

import Html (html_)
import Parser.Internal (Block (CodeBlock, Heading, OrderedList, Paragraph, QuoteBlock, UnorderedList), Inline (Bold, Code, Italic, ItalicBold, LineBreak, Plain), parse, render)
import System.Process (readProcess)

testAst =
  [ Heading 1 [Plain "This is a h1"],
    Heading 2 [Plain "This is a h2"],
    Heading 3 [Plain "This is a h3"],
    Heading 4 [Plain "This is a h4"],
    Heading 5 [Plain "This is a h5"],
    Heading 6 [Plain "This is a h6"],
    Paragraph [Plain "This is a paragraph"],
    Paragraph [Plain "This is another paragraph.", LineBreak, Plain "With a line break."],
    Paragraph [Plain "How about some ", Bold "Bold", Plain "? Or perhaps some ", Italic "italics", Plain "? ", ItalicBold "Both", Plain "?"],
    Paragraph [Plain "Maybe some inline ", Code "code", Plain "?"],
    CodeBlock "python" ["# Python more your style?", "print('Hellow, World!')"],
    QuoteBlock [Paragraph [Plain "Haskell is the world's finest imperative language."], Paragraph [Plain "Simon Peyton Jones"]],
    Paragraph [Plain "Pros of Haskell:"],
    UnorderedList
      [ Paragraph [Plain "Pure Functional Programming"],
        Paragraph [Plain "Strong, Static Type System"],
        UnorderedList
          [ Paragraph [Plain "nested"],
            Paragraph [Plain "list"],
            UnorderedList
              [ Paragraph [Plain "nested"],
                Paragraph [Plain "list"]
              ]
          ]
      ],
    OrderedList
      [ Paragraph [Plain "Steep Learning Curve"],
        Paragraph [Plain "Laziness Can Be Tricky"],
        OrderedList
          [ Paragraph [Plain "nested"],
            Paragraph [Plain "list"]
          ]
      ]
  ]

-- evil jank
main :: IO ()
main = do
  raw <- readFile "content/index.md"
  let ast = parse raw
  mapM_ print ast
  let page = html_ "my title" (render ast)
  -- formatted <- readProcess "tidy" ["-indent", "-quiet"] (show page)
  writeFile "build/index.html" (show page)

-- page :: Html
-- page =
--   html_
--     "This is your title"
--     ( h_ 1 (escape "This is a h1")
--         <> h_ 2 (escape "This is a h2")
--         <> h_ 3 (escape "This is a h3")
--         <> h_ 4 (escape "This is a h4")
--         <> h_ 5 (escape "This is a h5")
--         <> h_ 6 (escape "This is a h6")
--         <> p_ (escape "This is a paragraph.")
--         <> p_
--           ( escape "This is another paragraph."
--               <> br_
--               <> escape "With a line break."
--           )
--         <> p_
--           ( escape "How about some "
--               <> strong_ (escape "bold? ")
--               <> escape "Or perhaps some "
--               <> em_ (escape "italics? ")
--               <> bi_ (escape "Both? ")
--           )
--         <> p_
--           ( escape "Maybe some inline "
--               <> code_ (escape "code")
--               <> escape "? "
--           )
--         <> codeBlock_ "python" (escape "# Python more your style?\nprint('Hello, World!')")
--         <> quote_
--           ( p_ (escape "\"Haskell is the world's finest imperative language.\"")
--               <> p_ (em_ (escape "— Simon Peyton Jones"))
--           )
--         <> hr_
--         <> p_ (escape "Pros of Haskell: ")
--         <> ol_
--           [ li_ (p_ (escape "Pure Functional Programming")),
--             li_ (p_ (escape "Strong, Static Type System")),
--             li_ (p_ (escape "Excellent Abstractions")),
--             li_ (p_ (escape "Great for Compilers and DSLs")),
--             li_ (p_ (escape "Lazy Evaluation")),
--             li_ (p_ (escape "High Performance (when tuned)")),
--             li_ (p_ (escape "Rich Ecosystem for Academic and Research-Grade Tools"))
--           ]
--         <> p_ (escape "Cons of Haskell: ")
--         <> ul_
--           [ li_ (p_ (escape "Steep Learning Curve")),
--             li_ (p_ (escape "Laziness Can Be Tricky")),
--             li_ (p_ (escape "Smaller Ecosystem")),
--             li_ (p_ (escape "Cryptic Error Messages")),
--             li_ (p_ (escape "Dependency Hell in Some Ecosystems")),
--             li_ (p_ (escape "Smaller Community and Hiring Pool"))
--           ]
--     )