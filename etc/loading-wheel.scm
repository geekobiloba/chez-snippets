;;
;; Old-school loading wheel
;;
(define-syntax loading-wheel
  (syntax-rules (:stop-when :msg-start :msg-end :speed)

    ([_ :stop-when stop-when :msg-start msg-start :msg-end msg-end :speed speed]
     (begin
       (display (string-append msg-start "  ")) ; 2 trailing spaces
       (let loop ([n 0])
         (cond (stop-when
                 (display #\backspace)
                 (display msg-end)
                 (newline))
               (else (when (= n 4) (loop 0)) ; reset the counter
                     (display #\backspace)
                     (display #\backspace)
                     (display (vector-ref '#(#\/ #\- #\\ #\|) (mod n 4)))
                     (display #\space)
                     (flush-output-port) ; flush the buffer
                     (sleep :sec (/ 1 speed))
                     (loop (1+ n)))))))

    ([_ :stop-when st :msg-start m0 :msg-end m1]
     (loading-wheel :stop-when st :msg-start m0 :msg-end m1 :speed 8))

    ([_ :stop-when st :msg-start m0 :speed s]
     (loading-wheel :stop-when st :msg-start m0 :msg-end "" :speed s))

    ([_ :stop-when st :msg-start m0]
     (loading-wheel :stop-when st :msg-start m0 :msg-end "" :speed 8))

    ([_ :stop-when st :speed s]
     (loading-wheel :stop-when st :msg-start "" :msg-end "" :speed s))

    ([_ :msg-start m0 :speed s]
     (loading-wheel :stop-when #f :msg-start m0 :msg-end "" :speed s))

    ([_ :msg-end m1 :speed s]
     (loading-wheel :stop-when #f :msg-start "" :msg-end m1 :speed s))

    ([_ :msg-start m0]
     (loading-wheel :stop-when #f :msg-start m0 :msg-end "" :speed 8))

    ([_ :stop-when st]
     (loading-wheel :stop-when st :msg-start "" :msg-end "" :speed 8))

    ([_ :speed s]
     (loading-wheel :stop-when #f :msg-start "" :msg-end "" :speed s))

    ([_]
     (loading-wheel :stop-when #f :msg-start "" :msg-end "" :speed 8))))

