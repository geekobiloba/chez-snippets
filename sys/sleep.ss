;;
;; sleep extended syntax, like Unix sleep
;; 

;; (sleep timeobj)
;; (sleep :sec num)
;; (sleep* num)
;;
;; num must be rational number.
;; timeobj must be a time object with type time-duration.
;; The starred version is a shorthand for (sleep :sec num).
;;
(alias ori:sleep sleep)
(extend-syntax (sleep :sec)
               ([_ t] (ori:sleep t)) ; the original sleep syntax

               ([_ :sec num] (ori:sleep
                             (make-time
                               'time-duration
                               (inexact->exact (round (* (mod num 1) 1e9)))
                               (inexact->exact (round n))))))

(define (sleep* num) (sleep :sec num))

