;;
;; Run system commands
;;
;; Return a list of 2 elements: stdout and stderr.
;;
(define (system* cmd)
  (let-values
    ([(in out err pid)
      (open-process-ports cmd 'line (native-transcoder))])
    (list (get-string-all out)
          (get-string-all err))))

