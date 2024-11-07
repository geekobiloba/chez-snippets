;;
;; echo like in Unix
;;
;; (echo [:nonl] [:out port] args ...)
;;
;; :nonl      Don't append output with a newline.
;;
;; :out port  Output port.
;;
;; Every echo form produces a string with a trailing space.
;;
(define-syntax echo
  (syntax-rules (:nonl :out)

    ([_ :nonl :out port args ...]
     (for-each
       (lambda (e)
         (display e port)
         (display #\space port))
       (list args ...)))

    ([_ :out port args ...]
     (begin
       (echo :nonl :out port args ...)
       (newline port)))

    ([_ :nonl args ...]
     (for-each
       (lambda (e)
         (display e)
         (display #\space))
       (list args ...)))

    ([_ args ...]
     (begin
       (echo :nonl args ...)
       (newline)))))

