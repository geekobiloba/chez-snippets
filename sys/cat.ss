;;
;; Show file content, like Unix cat
;;

;; (cat :out port file)
;; (cat :human file)
;; (cat* file)
;;
;; :out port  Output port.
;;
;; The starred version is a shorthand for (cat :human)
;;
(define-syntax cat
  (syntax-rules (:out :human)

    ;; Return string
    ([_ file]
     (call-with-input-file file get-string-all))

    ([_ :out port file]
     (display-string (call-with-input-file file get-string-all) port))

    ;; Return none, for human
    ([_ :human file]
     (display-string (call-with-input-file file get-string-all)))))

;; Shorthand for human
(define (cat* file) (cat :human file))

