;;; cscomp-base.el -- part of cscomp, which provides
;;;                   Smart code completion for C#
;;
;; Author:     Dino Chiesa <dpchiesa@hotmail.com>
;; Maintainer: Dino Chiesa <dpchiesa@hotmail.com>
;; Created:    April 2010
;; Modified:   January 2011
;; Version:    0.1.9
;; Keywords:   c# languages oop mode
;; X-URL:      http://code.google.com/p/csharpmode/
;;


(defcustom cscomp-log-level 1
  "The current log level for C# completion operations.
0 = NONE, 1 = Info, 2 = VERBOSE, 3 = DEBUG. ")

(defun cscomp-time ()
  "returns the time of day as a string.  Used in the `cscomp-log' function."
  (substring (current-time-string) 11 19)) ;24-hr time


(defun cscomp-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `cscomp-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (if (<= level cscomp-log-level)
      (let* ((msg (apply 'format text args)))
        (message "CSCOMP %s %s" (cscomp-time) msg))))

(provide 'cscomp-base)

;; End of cscomp-base.el
