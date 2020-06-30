(use vegex rewind-ports vunit)

(define (open-rewind-string x)
  (port->rewind-port (open-input-string x)))
(test-suite
  (begin-tests compilation
    #//
    #/A+/
    #/abc/
    #/(mystery)?/
    #/[8675309][^\s1-9]*/
    #/\t\n\r\s\d\w/
    #/(absolute)??/
    #/(((a)))/
    #/.\d+/
    #/\<brackets\>/
    #/\bbor\Bing\b/
    #/some|none/
    #/(some|no)body/
    #/"\/"/
    #/[\/]/
    #/ f o o /
    #/ f ? ?/
    #/^In the beginning was the Word\.$/)
  (begin-tests execution
    (#// (open-rewind-string "nothing"))
    (#/[\/]/ (open-rewind-string "/"))
    (#/foo/ (open-rewind-string "foo"))
    (let ((port (open-rewind-string "you need")))
      (and
        (not (#/you want/ port))
        (#/you / port)))
    (#/some|none/ (open-rewind-string "some"))
    (#/(a|ab)ject/ (open-rewind-string "abject"))
    (let ((port (open-rewind-string "consumption")))
      (and
        (#/.+/ port)
        (not (#/./ port))))
    (#/^My only friend, the end$/ (open-rewind-string "My only friend, the end"))
    (let ((port (open-rewind-string "suffering\nabsolute")))
      (and (#/\<suffering\>/ port)
           (#/\n/ port)
           (#/^ab\Bsolute$/ port)))
    (not (#/y*y*z/ (open-rewind-string "yyyyy")))
    (#/z?y*y*z/ (open-rewind-string "zyyyyy"))
    (let ((port (open-rewind-string "xxx")))
      (and
        (#/.+?/ port)
        (#/.+?/ port)
        (#/.+?/ port)
        (not (#/.+?/ port)))))
        
  (begin-tests configuration
    (parameterize ((atmosphere '(#\_))
                   (delimiters '()))
      (#/\<cloud\>/ (open-rewind-string "cloud_")))
    (let ((a #/old\>/))
      (parameterize ((atmosphere '(#\_))
                     (delimiters '()))
        (and (a (open-rewind-string "old;"))
             (not (#/old\>/ (open-rewind-string "old;")))
             (#/new\B\s\>/ (open-rewind-string "new _")))))
  )
)
