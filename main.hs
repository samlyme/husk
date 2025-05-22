{-# OPTIONS_GHC -Wall #-}

import Html.Internal
  ( Attribute,
    Html,
    Structure (..),
    Title,
    attr,
    br_,
    code_,
    ela,
    em_,
    h_,
    html_,
    ol_,
    p_,
    quote_,
    render,
    strong_,
    ul_,
  )

main :: IO ()
main = writeFile "build/index.html" (render page)

test :: String
test = ela "test" [attr "class" "lmao", attr "id" "balls"] (p_ "lmao")

page :: Html
page =
  html_
    "my title"
    ( code_ "print('hello world')"
        <> quote_ (h_ 1 "Quote" <> p_ "blah")
        <> ( p_ "p1"
               <> br_
               <> p_ "p2"
               <> Structure test
               <> ul_
                 [ p_ "list",
                   p_ "lmao",
                   ol_ [p_ "ordered", p_ "lol"],
                   --  problem: input should either be escaped text
                   --  or an html structure. Should be solved using a data type.
                   --  Weird styles can be acheived. I will just let it be.
                   p_ em_ "italics"
                 ]
           )
    )
