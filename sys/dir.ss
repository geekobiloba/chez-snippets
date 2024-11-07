;;
;; List directory contents, default to current directory.
;;
;; (dir [:rec] [path))
;;
;; Default to current directory.
;; Directory names will have a trailing directory separator character.
;;
(define-syntax dir
  (syntax-rules (:rec)

    ([_] (dir (current-directory)))

    ([_ :rec] (dir :rec (current-directory)))

    ([_ path]
     (map (lambda (str)
            (if (file-directory? str)
              (string-append str (path-build "" "")) ; / on Unix, \ on Windows
              str))
          (directory-list path)))

    ([_ :rec path]
     (map (lambda (str)
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

