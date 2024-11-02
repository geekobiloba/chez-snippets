;;
;; Trim leading and trailing whitespaces
;;
(define (string-trim str)
  (let loop ([lst (string->list str)]
             [rev 0])
    (cond ([null? lst] "")

          ([char-whitespace? (car lst)]
           (loop (cdr lst) rev))

          ([and (not (char-whitespace? (car lst)))
                (< rev 2)]
           (loop (reverse lst) (1+ rev))) ; stop reversing at 2

          (else (list->string lst)))))

