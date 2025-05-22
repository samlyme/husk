# husk

markdown -> html, written in haskell

## idea history
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