;;
;; (string-split [:str] str sepstr)
;; (string-split [:char] str sepchar)
;; (string-split str otagstr ctagstr)
;;
;; Split string by character, string, or tag pair.
;;
;; The third form is still ugly and probably unreliable!
;;
(define-syntax string-split
  (syntax-rules (:char :str)

    ([_ str sep]
     (cond ([char?   sep] (string-split :char str sep))
           ([string? sep] (string-split :str  str sep))))

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
                          (cons (substring str i l) '())))))))

    ;; Split string by enclosing and closing tags
    ;; Still ugly and probably unreliable!
    ([_ str otag ctag]
     (let
       ([l (string-length str)]
        [m (string-length otag)]
        [n (string-length ctag)])
       (let loop ([i 0] ; substring start index
                  [j 0] ; substring end index
                  [p m]       ; otag walker
                  [q (+ m n)] ; ctag walker
                  [p-score 0]
                  [q-score 0])

               ;; Return empty list if str length is shorter
               ;; than the sum of otag and csep lengths.
         (cond ([<= l (+ m n)] '())

               ;; TODO: What should be the return value
               ;;       for empty either opening or closing tags?

               ;; Empty separators yields i list of every character in the string.
               ([zero? (+ m n)] (map string (string->list str)))

               ([<= q l] (cond ([string=? (substring str (- p m) p) otag]
                                (if (zero? p-score)
                                  (loop p j (1+ p) q (1- p-score) q-score)
                                  (loop i j (1+ p) q (1- p-score) q-score)))

                               ([string=? (substring str (- q n) q) ctag]
                                (loop i q p (1+ q) p-score (1+ q-score)))

                               ([and (zero? (+ p-score q-score))
                                     (< p-score 0)
                                     (> q-score 0)]
                                (cons (substring str i (- j n))
                                      (loop p q (1+ p) (1+ q) 0 0)))

                               (else (loop i j (1+ p) (1+ q) p-score q-score))))

               (else (if (zero? (+ p-score q-score))
                       (cons (substring str i (- j n)) '())
                       '()))))))))

;; Clea version, remove any empty string element
(define (string-split* str sep0 . sep1)
  (if (null? sep1)
    (remove "" (string-split str sep0))
    (remove "" (string-split str sep0 (car sep1)))))

