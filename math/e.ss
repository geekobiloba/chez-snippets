;;
;; Calculate e number recursively with Taylor series
;;

(define factorial
  (lambda (n)
    (if (= n 0)
      1
      (* n (factorial (- n 1))))))

(define e
  (inexact
    (let taylor ([c 0] [s 0]) ; counter & sum
      (let [(t (/ 1 (factorial c)))] ; term of the series
        (if (zero? (inexact t))
          s
          (taylor (+ c 1) (+ s t)))))))

;; To trace:
;;
;;   1. Turn on tracing  : (trace e*)
;;   2. Call the function: (e* 0 0)
;;   3. Turn off tracing : (untrace e*)
;;
(define (e* c s)
  (inexact
    (let [(t (/ 1 (factorial c)))] ; term of the series
      (if (zero? (inexact t))
        s
        (e* (+ c 1) (+ s t))))))

