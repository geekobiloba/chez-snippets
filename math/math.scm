;;
;; Maths shorthands
;;

;; Constants
(define math:e   (exp 1))
(define math:pi  (* 2 (asin 1)))
(define math:phi (list (/ (- 1 (sqrt 5)) 2)
                       (/ (+ 1 (sqrt 5)) 2)))

;; Trigonometry in degrees
(alias ori:sin sin)
(alias ori:cos cos)
(alias ori:tan tan)
(extend-syntax (sin :deg)
               ([_ n] (ori:sin n))
               ([_ :deg n] (ori:sin (* n (/ math:pi 180)))))
(extend-syntax (cos :deg)
               ([_ n] (ori:cos n))
               ([_ :deg n] (ori:cos (* n (/ math:pi 180)))))
(extend-syntax (tan :deg)
               ([_ n] (ori:tan n))
               ([_ :deg n] (ori:tan (* n (/ math:pi 180)))))

;; Logarithm with custom base
(alias ori:log log)
(extend-syntax (log :base :10)
               ([_ n] (ori:log n))
               ([_ :base b n] (/ (ori:log n) (ori:log b)))
               ([_ :10 n] (log :base 10 n)))

