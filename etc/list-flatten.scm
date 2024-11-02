;;
;; Flatten nested list
;;
;; Stolen from http://markmiyashita.com/cs61a/scheme/flatten_nest_lists/
;;
;; All copyright of this file belongs to him.
;;
(define (list-flatten lst)
  (cond [(null? lst) lst]
        [(list? (car lst))
         (append (list-flatten (car lst))
                 (list-flatten (cdr lst)))]
        [else (cons (car lst)
                    (list-flatten (cdr lst)))]))

