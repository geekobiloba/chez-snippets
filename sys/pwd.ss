;;
;; pwd like in Unix
;;
;; (pwd)
;;
;; (current-directory) can take an argument and acts like cd,
;; but Unix pwd shouldn't take any.
;;
(define (pwd) (current-directory))

