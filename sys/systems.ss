;;
;; Scheme shortands for system administrations
;;
;; TODO:
;; - copy
;; - touch
;; - find -> :start, :end
;; - uniq
;; - save-history -> :last :first :input-file :history-file : output-file :file
;; - edit -> (system "vim ~/.chezscheme_history")

;; Each can only takes two args: path and error return
(alias mv    rename-file)
(alias rm    delete-file)
(alias rmdir delete-directory)

;; (pwd)
;;
;; (current-directory) can take an argument and acts like cd,
;; but Unix pwd shouldn't take any.
;;
(define (pwd) (current-directory))

;; (dir [:rec] [path))
;;
;; List directory contents,
;; default to current directory.
;;
;; Listed directory names will have a trailing directory separator character.
;;
(define-syntax dir
  (syntax-rules (:rec)

    ([_] (dir (current-directory)))

    ([_ :rec] (dir :rec (current-directory)))

    ([_ path]
     (map
       (lambda (str)
         (cond
           ([file-directory? str]
            (string-append str (path-build "" ""))) ; / on Unix, \ on Windows
           (else str)))
       (directory-list path)))

    ([_ :rec path]
     (map
       (lambda (str)
         (if (file-directory? str)
           (string-append str (path-build "" ""))
           str))
       (let loop-path-str ([path-str path])
         (if (file-directory? path-str)
           (let loop-dir-list ([dir-list (directory-list path-str)])
             (if (null? dir-list)
               dir-list
               (let ([c-path-str (path-build path-str (car dir-list))])
                 (cons c-path-str
                       (append (loop-path-str c-path-str)
                               (loop-dir-list (cdr dir-list)))))))
           '()))))))

;; Shorthand for recursive dir
(define (dir* . path)
  (if (null? path)
    (dir :rec)
    (dir :rec (car path))))

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

;; Run system commands
;;
;; Return a list of 2 elements: stdout and stderr
(define (system* cmd)
  (let-values
    ([(in out err pid)
      (open-process-ports cmd 'line (native-transcoder))])
    (list (get-string-all out)
          (get-string-all err))))

