;;
;; Scheme shortands for system administrators
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

