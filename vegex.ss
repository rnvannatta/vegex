(module vegex
  (parse regex-table compile-regex parse-regex make-dripqueue depop depeek depush end-of-dripqueue? end-of-dripqueue port->dripqueue atmosphere delimiters)
  (import chicken scheme srfi-1 vpatterns ports extras rewind-ports)
(use srfi-1 vpatterns ports rewind-ports)

(define (make-dripqueue generator)
  (list '() generator))
(define (depop dripqueue)
  (if (null? (car dripqueue))
      ((cadr dripqueue))
      (let ((ret (caar dripqueue)))
        (set-car! dripqueue (cdar dripqueue))
        ret)))
(define (depeek dripqueue)
  (if (null? (car dripqueue))
      (begin (depush (depop dripqueue) dripqueue)
             (depeek dripqueue))
      (caar dripqueue)))
(define (depush value dripqueue)
  (set-car! dripqueue (cons value (car dripqueue))))
(define-record-type end-of-dripqueue
  (make-end-of-dripqueue)
  end-of-dripqueue?)
(define end-of-dripqueue (make-end-of-dripqueue))
(define (port->dripqueue port)
  (make-dripqueue
    (lambda ()
      (let ((it (read-char port)))
        (if (eof-object? it)
            end-of-dripqueue
            it)))))

(define (make-stack)
  (vector '()))
(define (stack-push! stack val)
  (vector-set! stack 0 (cons val (vector-ref stack 0))))
(define (stack-pop! stack)
  (let ((ret (car (vector-ref stack 0))))
    (vector-set! stack 0 (cdr (vector-ref stack 0)))
    ret))

; TODO remove that stupid dripqueue
(define (parse rule-table dripqueue token? lexeq?)
  (define (unread-parsing seg)
    (cond
      ((token? seg) (depush seg dripqueue))
      ((null? seg) #f)
      (else (unread-parsing (cdr seg)) (unread-parsing (car seg)) #f)))
  (define (production? rule)
    (and (pair? rule)
         (pair? (cdr rule))
         (null? (cddr rule))
         (eq? (car rule) 'quote)
         (cadr rule))
    #;(matches? ''production rule ))
  (define (begin-parse rule)
    (cond
      ((production? rule)
       (let ((next (cadr (assoc (cadr rule) rule-table))))
         (if (vector? next)
             (iter-seq next 0)
             (iter-options next))))
      ((list? rule) (iter-options rule))
      ((vector? rule) (iter-seq rule 0))))
  (define (parse-match? rule)
    (cond ((end-of-dripqueue? (depeek dripqueue)) #f)
          #;((member (depeek dripqueue) '(#\space #\tab #\newline))
           (depop dripqueue) (parse-match? rule))
          ((or (char? rule) (symbol? rule))
           (if (lexeq? (depeek dripqueue) rule)
               (depop dripqueue)
               #f))
          (else (begin-parse rule))))
  (define (iter-options options)
    (cond
      ((null? options) #f)
      (else
        (let ((parse-match (parse-match? (car options))))
          (if parse-match
              parse-match
              (iter-options (cdr options)))))))
  (define (iter-seq seq state)
    (if (>= state (vector-length seq))
        '()
        (let* ((cur (vector-ref seq state))
               (rep (eq? cur '*))
               (cur (if rep (vector-ref seq (+ state 1)) cur))
               (parse-match (parse-match? cur)))
          (if parse-match
              (let ((ret (iter-seq seq (+ state (if rep 0 1)))))
                (if ret
                    (cons parse-match ret)
                    (begin (unread-parsing parse-match) #f)))
              (if rep
                  (iter-seq seq (+ state 2))
                  #f)))))
  (begin-parse ''start))

(define (char-span first last)
  (let ((first (char->integer first))
        (last (char->integer last)))
    (map integer->char (iota (- last first -1) first))))
; TODO compile character or's into lookup tables
; XXX \ should escape everything
; TODO curly {}
(define regex-table
  `((plainchar (,@(char-span #\a #\z)
                ,@(char-span #\A #\Z)
                ,@(char-span #\0 #\9)
                #\space))
	  (specialclass #(#\\ (#\t #\n #\s #\d #\w #\r)))
    (rawchar ('plainchar
              #\! #\@ #\# #\% #\& #\- #\_ #\= #\] #\}
              #\; #\: #\' #\, #\< #\> #\" #\` #\~))
    (rawclass ('plainchar
               #\! #\@ #\# #\% #\& #\_ #\= #\}
               #\; #\: #\' #\, #\< #\> #\" #\` #\~
               #\( #\) #\[ #\. #\* #\+ #\? #\$ #\| #\{ ))
    (classchar ('rawclass 'specialclass #(#\\ (#\" #\\ #\] #\- #\^ #\/))))
    (keyword (#\( #\) #\[ #\. #\* #\+ #\? #\\ #\^ #\| #\{ #\/ #\$))
    (any (#\.))
    (end (#\$))
    (beginning (#\^))
    (escaped #(#\\ 'keyword))
    (char ('escaped 'specialclass 'rawchar #(#\\ (#\b #\B #\< #\>))))
    (between #('classchar #\- 'classchar))
    (noneof #(#\[ #\^ * ('between 'classchar) #\]))
    (anyof #(#\[ * ('between 'classchar) #\]))

    (basic-expr ('any 'anyof 'noneof 'group 'char))
    (expr ('lookahead 'end 'beginning #('basic-expr (#((#\? #\+ #\*) #\?) #\? #\+ #\*)) 'basic-expr))
    (regex-meat ('option 'expr))
    (option #(#('expr * 'expr) #\| #('regex-meat * 'regex-meat)))

    (lookahead #(#\( #\? #\= * 'regex-meat #\)))
    (group #(#\( * 'regex-meat #\)))
    (start #(* 'regex-meat #\/))))

(define (backtrack port output backtracks)
  (let ((backtrack (stack-pop! backtracks)))
        (rewind-port-seek! port (car backtrack))
        ((cdr backtrack) #t)))
(define (mark-backtrack port backtracks)
  (call/cc
    (lambda (k)
      (stack-push! backtracks (cons (save-rewind-port port) k))
      #f)))

(define (read-or-backtrack f)
  (lambda (port output backtracks)
    (if (let ((c (peek-char port)))
          (and (not (eof-object? c)) (f c)))
        (cons (read-char port) output)
        (backtrack port output backtracks))))

;(define atmosphere '(#\tab #\space #\newline))
;(define delimiters '(#\( #\) #\" #\;))
(define atmosphere (make-parameter '(#\tab #\space #\newline #\return)))
(define delimiters (make-parameter '(#\( #\) #\" #\;)))

(define (boundary left right)
  (lambda (port output backtracks)
    (if (let ((b (rewind-port-lookbehind port))
              (c (peek-char port)))
          (or
            ; line indicates the cursor
            ; '|word' 'word|(' 'a |word'
            (and left
              (not (member c (atmosphere)))
              (or (not b)
                  (member b (delimiters))
                  (member b (atmosphere))))
            ; line indicates the cursor
            ; 'word|' ')|' 'a| word'
            (and right
              (not (member b (atmosphere)))
              (or (member c (delimiters))
                  (member c (atmosphere))
                  (eof-object? c)))))
        output
        (backtrack port output backtracks))))
(define (notboundary)
  (lambda (port output backtracks)
    (if (let ((b (rewind-port-lookbehind port))
              (c (peek-char port)))
          (or (not b)
              (member b (atmosphere))
              (member b (delimiters))
              (member c (atmosphere))
              (member c (delimiters))
              (eof-object? c)))
        (backtrack port output backtracks)
        output)))

(define (anychar)
  (read-or-backtrack (lambda (e) #t)))
(define (end)
  (lambda (port output backtracks)
    (cond ((eof-object? (peek-char port)) output)
          ((eq? (peek-char port) #\newline) (cons (read-char port) output))
          (else (backtrack port output backtracks)))))
(define (beginning)
  (lambda (port output backtracks)
    (cond ((not (rewind-port-lookbehind port)) output)
          ((eq? (rewind-port-lookbehind port) #\newline) output)
          (else (backtrack port output backtracks)))))
(define (exactly char)
  (read-or-backtrack (cut eq? <> char)))

(define (anyof-test e chars ranges)
  (or (member
        (char->integer e)
        ranges
        (lambda (e range) (<= (car range) e (cadr range))))
      (member e chars)))
; TODO table
(define (anyof chars ranges)
  (read-or-backtrack
    (cut anyof-test <> chars ranges)))
(define (noneof chars ranges)
  (read-or-backtrack
    (lambda (e) (not (anyof-test e chars ranges)))))

(define (option action-a action-b)
  (lambda (port output backtracks)
    (if (mark-backtrack port backtracks)
        (action-b port output backtracks)
        (action-a port output backtracks))))

(define (potentially action)
  (lambda (port output backtracks)
    (if (mark-backtrack port backtracks)
        output
        (action port output backtracks))))
(define (many action)
  (lambda (port output backtracks)
    (let loop ((output output))
      (if (mark-backtrack port backtracks)
          output
          (loop (action port output backtracks))))))
(define (more action)
  (lambda (port output backtracks)
    ((lambda (output) ((many action) port output backtracks))
     (action port output backtracks))))

(define (lazy-potentially action)
  (lambda (port output backtracks)
    (if (not (mark-backtrack port backtracks))
        output
        (action port output backtracks))))
(define (lazy-many action)
  (lambda (port output backtracks)
    (let loop ((output output))
      (if (not (mark-backtrack port backtracks))
          output
          (loop (action port output backtracks))))))
(define (lazy-more action)
  (lambda (port output backtracks)
    ((lambda (output) ((lazy-many action) port output backtracks))
     (action port output backtracks))))

(define (group . actions)
  (lambda (port output backtracks)
    (let loop ((actions actions) (output output))
      (if (null? actions)
          output
          ((lambda (output) (loop (cdr actions) output))
           ((car actions) port output backtracks))))))
; I don't like lookahead
(define (lookahead . actions)
  (let ((group (apply group actions)))
    (lambda (port output backtracks)
      ;after a successful lookahead, we backtrack to before
      ;the lookahead and return the original output
      ; XXX this call/cc feels totally unnecessary
      (let* ((backtrack #f)
             (cursor (save-rewind-port port)))
        (if (call/cc (lambda (k) (set! backtrack k) #f))
            output
            ((lambda (new-output)
               (rewind-port-seek! port cursor)
               (backtrack #t))
             (group port output backtracks)))))))

(define numspan `(,(char->integer #\0) ,(char->integer #\9)))
(define lowerspan `(,(char->integer #\a) ,(char->integer #\z)))
(define upperspan `(,(char->integer #\A) ,(char->integer #\Z)))
(define (anyof-body exprs)
  (let loop ((chars '()) (ranges '()) (exprs exprs))
    (if (null? exprs)
        `(,chars ,ranges)
        (match (car exprs)
          ((#\\ x)
           (case x
             ((#\- #\^ #\] #\\ #\" #\/) (loop (cons x chars) ranges (cdr exprs)))
             ((#\n) (loop (cons #\newline chars) ranges (cdr exprs)))
             ((#\t) (loop (cons #\tab chars) ranges (cdr exprs)))
             ((#\s)
              (loop (append '(#\tab #\newline #\space) chars) ranges (cdr exprs)))
             ((#\d) (loop chars (cons numspan ranges) (cdr exprs)))
             ((#\w) (loop chars
                          (append `(,numspan ,lowerspan ,upperspan) ranges)
                          (cdr exprs)))
             (else (loop chars ranges (cdr exprs)))))
          ((x #\- y)
           (loop chars
                 (cons `(,(char->integer x) ,(char->integer y)) ranges)
                 (cdr exprs)))
          (x (loop (cons x chars) ranges (cdr exprs)))))))

(define (compile-regex ast)
  (let ((atmos (atmosphere))
        (delim (delimiters))
        (regex
          (let loop ((ast ast))
            (case (car ast)
              ;terminal leaves
              ((exactly) (exactly (cadr ast)))
              ((anychar) (anychar))
              ((end) (end))
              ((beginning) (beginning))
              ((anyof) (apply anyof (cdr ast)))
              ((noneof) (apply noneof (cdr ast)))
              ((boundary) (boundary (cadr ast) (caddr ast)))
              ((notboundary) (notboundary))
              ;intermediary branches
              ((potentially) (potentially (loop (cadr ast))))
              ((many) (many (loop (cadr ast))))
              ((more) (more (loop (cadr ast))))

              ((lazy-potentially) (lazy-potentially (loop (cadr ast))))
              ((lazy-many) (lazy-many (loop (cadr ast))))
              ((lazy-more) (lazy-more (loop (cadr ast))))

              ((option) (apply option (map loop (cdr ast))))
              ((group) (apply group (map loop (cdr ast))))
              ((lookahead) (apply lookahead (map loop (cdr ast))))
              (else (error "Unknown form in regex cst" ast))))))
     (lambda (port)
      (parameterize ((atmosphere atmos) (delimiters delim))
        (let ((backtracks (vector '())))
          (if (mark-backtrack port backtracks)
              #f
              (reverse 
                (regex port
                       '()
                       backtracks))))))))


; TODO
; possessive quantifier +
(define (parse-regex dripqueue)
  (let ((expr (parse regex-table dripqueue char? eq?)))
    (if (not expr) (error "Failed to parse regular expression"))
    (let loop ((expr expr))
      (match expr
        ;terminal
        (#\. '(anychar))
        (#\$ '(end))
        (#\^ '(beginning))
				((#\\ #\s) `(anyof (#\tab #\newline #\space) ()))
				((#\\ #\w) `(anyof (#\_) (,numspan ,lowerspan ,upperspan)))
				((#\\ #\d) `(anyof () (,numspan)))
				((#\\ #\t) `(exactly #\tab))
				((#\\ #\n) `(exactly #\newline))
				((#\\ #\r) `(exactly #\return))
        ((#\\ #\b) `(boundary #t #t))
        ((#\\ #\<) `(boundary #t #f))
        ((#\\ #\>) `(boundary #f #t))
        ((#\\ #\B) `(notboundary))
        ((#\\ x) `(exactly ,x))
        ;more looping
        ((expr #\?) `(potentially ,(loop expr)))
        ((expr #\*) `(many ,(loop expr)))
        ((expr #\+) `(more ,(loop expr)))
        ((expr (#\? #\?)) `(lazy-potentially ,(loop expr)))
        ((expr (#\* #\?)) `(lazy-many ,(loop expr)))
        ((expr (#\+ #\?)) `(lazy-more ,(loop expr)))
        ((expr-a #\| expr-b) `(option (group . ,(map loop expr-a))
                                      (group . ,(map loop expr-b))))
        ((#\( #\? #\= exprs ... #\)) `(lookahead . ,(map loop exprs)))
        ((#\( exprs ... #\)) `(group . ,(map loop exprs)))
        ((#\[ #\^ exprs ... #\]) `(noneof . ,(anyof-body exprs)))
        ((#\[ exprs ... #\]) `(anyof . ,(anyof-body exprs)))
        ((exprs ... #\/)
         `(group . ,(map loop exprs)))
        (else `(exactly ,expr))))))

(set-sharp-read-syntax! #\/
  (lambda (port)
    (let* ((dripqueue (port->dripqueue port))
           (ret (parse-regex dripqueue)))
      `(compile-regex ',ret))))

)
