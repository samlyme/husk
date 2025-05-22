# husk

markdown -> html, written in haskell

## TODO:
1. markdown parser
2. make ul and ol take in a li, so that some objects can be "escaped"
3. project file structure
4. set up cabal
5. make this into a proper cli app, maybe use some jank bash scripts at first
6. publish to hackage

## idea history
Need to make a config file to define the style and code highlighting theme.

Maybe I can implement a "wrapper" for everything that just passes in the calls the escape function for me. 

I wanted to keep everythin clean, so at first, I created separate types for strings that have been escaped to HTML, and actual HTML structures. 
However, as I developed the library, I releazed that an escaped string and an HTML structure were functionally identical. However, it is still useful to keep a type "EscapedString" because it gives a hint as to the uses. Although functionally equivalent, you would not want to pass in an actual html structure into a code block (usually). 

## devlog
- early dev, decided on supporting "basic sytanx" in markdown
  - code blocks should be fenced
  - horizontal rules
  - us semantic html like <strong> and <em>
- mainly building out the required html
- code syntax highlighting should be done with highlight.js

### Problems
- input should either be escaped text or an html structure. Should be solved using a data type. Weird styles can be acheived. I will just let it be.
- escaping text should not be the responsibility of the html library
- honestly, the whole "Internal.hs" pattern is too much