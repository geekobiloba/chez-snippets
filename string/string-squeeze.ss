;;
;; (string-squeeze str [greedy?])
;;
;; Squeeze whitespace in string.
;; The greedy option squeezes all contiguous whitespaces regardlessly.
;;
(define-syntax string-squeeze
  (syntax-rules ()

    ([_ str greedy?]
     (list->string
       (let loop ([prev #\nul]
                  [lst (string->list str)])
         (cond ([null? lst] lst)

               ([and (char-whitespace? (car lst))
                     (char-whitespace? prev)
                     (or (boolean=? greedy? #t)
                         (char=? prev (car lst)))]
                (loop (car lst) (cdr lst)))

               (else (cons (car lst)
                           (loop (car lst) (cdr lst))))))))

    ([_ str]
     (string-squeeze str #t)))) ; the default is to be greedy

