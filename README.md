# husk

markdown -> html, written in haskell

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