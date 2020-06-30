# vegex
Regular Expression Library for Chicken Scheme

This is a regular expression library that works a little differently from most in Lisp. Instead of using an AST-style regex, it utilizes reader macros to achieve typical syntax for regular expression and compile it to code. This requires my other two chicken libraries, vpatterns and rewind-ports.

The regular expressions compile to procedures that accept rewindinable ports (see my other library) of input. They start matching at the current cursor of the port and if they fail mid-match, the cursor is restored to the state it was before the regex was run. Example usage:

```
; Matches a number which is a delimited token that is one or more digits, with an optional decimal place,
; optional digits to the left of the decimal place, and an optional + or - on the front.
(define number-matcher #/\b(\+|-)?\d*\.?\d+/\b)
(number-matcher (port->rewind-port (open-input-string "-6.12 dollars"))) ; returns '(#\- #\6 #\. #\.1 #\2) and the port's next character at the space
(number-matcher (port->rewind-port (open-input-string "-6x"))) ; returns #f and despite the partial match, the port's cursor is still at the negative sign
```
