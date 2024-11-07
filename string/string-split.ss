;;
;; (string-split [:strict] str sep)
;; (string-split [:strict] [:str] str sepstr)
;; (string-split [:strict] [:char] str sepchar)
;;
;; Split string by character or string.
;; The :strict option returns empty list when no separator is found.
;;
(define-syntax string-split
  (syntax-rules (:strict :char :str)

    ([_ str sep]
     (cond ([char?   sep] (string-split :char str sep))
           ([string? sep] (string-split :str  str sep))))

    ([_ :strict str sep]
     (cond ([char?   sep] (string-split :strict :char str sep))
           ([string? sep] (string-split :strict :str  str sep))))

    ([_ :strict :char str char]
     (let ([res (car (string-split :char str char))])
       (if (string=?  str) '() res)))

    ([_ :strict :str str sep]
     (let ([res (car (string-split :str str sep))])
       (if (string=?  str) '() res)))

    ;; Split string by character
    ([_ :char str char]
     (let ([l (string-length str)])
       (let loop ([i 0]  ; substring start index
                  [p 0]) ; char walker
         (cond ([zero? l] '()) ; empty string yields an empty list.

               ([< p l] (if (char=? (string-ref str p) char)
                          (cons (substring str i p)
                                (loop (1+ p) (1+ p)))
                          (loop i (1+ p))))

               ([>= p l] (if (char=? (string-ref str (- p 1)) char)
                           (cons (substring str i p) '())
                           (cons (substring str i l) '())))))))

    ;; Split string by string
    ([_ :str str sep]
     (let ([l (string-length str)]
           [m (string-length sep)])
       (let loop ([i 0]  ; substring start index 
                  [p m]) ; sep walker
         (cond ([zero? l] '())

               ;; Empty separator yields a list
               ;; of every character in the string.
               ([zero? m] (map string (string->list str)))

               ([<= p l] (if (string=? (substring str (- p m) p) sep)
                           (cons (substring str i (- p m))
                                 (loop p (+ p m)))
                           (loop i (1+ p))))

               ([> p l] (if (string=? (substring str (- p m) l) sep)
                          (cons (substring str i (- p m)) '())
                          (cons (substring str i l) '())))))))))

