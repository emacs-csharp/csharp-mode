;;; csharp-shell.el --- run PowerShell to assist with c# completion.
;;
;; Author:     Dino Chiesa <dpchiesa@hotmail.com>
;; Created:    10 Apr 2008
;; Modified:   May 2010
;; Version:    0.2
;; Keywords:   powershell C# shell
;; X-URL:      ??
;;

;;; Commentary:
;;
;;   This is code that provides an emacs shell that supports C# code-completion.
;;   It depends on Powershell.el.  It is part of cscomp.  (C# completion).
;;
;;
;;   csharp-shell.el is responsible for starting a
;;   PowerShell shell, and loading a custom utility assembly into it.
;;   The running shell can be used as a regular, interactive powershell shell, but its
;;   primary purpose is to connect elisp to the .NET assembly that
;;   performs reflection.
;;
;;   When the user requests completion on a variable (etc), logic within
;;   csharp-completion.el sends a command to the CscompShell, and gets the
;;   response, in the form of a lisp s-expression.  The logic in
;;   csharp-completion.el then evals that sexp, and does something
;;   intelligent with the result.
;;
;;   The two main public interfaces for this module are:
;;
;;    `csharp-shell-exec-and-eval-result'
;;    `csharp-shell-exec-and-maybe-eval-result'
;;
;;
;;   Some History:
;;   idea of CSDE, the C# Development Environment, was initially
;;   conceived by Matt Bruce in 2001 (or so), and ported from JDE, the
;;   Java Development Environment.
;;
;;   But the ambitious vision of CSDE was never completed. The latest
;;   version is aparently still available on sourceforge, but it was
;;   never updated or maintained.  <URL:http://www.sourceforge.com/>.
;;
;;   Rather than start with "everything", I thought I'd start by
;;   producing a module that did  one thing well: code completion.
;;   This is known as Intellisense in Microsoft's Visual Studio.
;;
;;   To do this, I didn't keep any of the old CSDE code. I kept only the
;;   *idea* of code completion, relying on the use of a shell.
;;
;;   ------------
;;
;;   Please send any comments, bugs, or upgrade requests to
;;   Dino Chiesa (dpchiesa@hotmail.com)
;;


(require 'powershell)

(defcustom csharp-shell-location-of-util-dll  nil
  "Folder name that contains the Cscomp DLL for C# Completion.
Set this to nil, to load the DLL named CscompUtilities.dll from
the same directory where csharp-shell.el is located.

Otherwise, set it to a fully-qualified path of a directory that contains
the file cscompUtilities.dll.  For example,

      \"c:\\users\\fred\\elisp\\cscomp\"

"
  :group 'cscomp
  :type 'string)

(defcustom csharp-shell-startup-timeout 20
  "*Length of time the CSharpShell waits for the shell process to startup.
Increase the value of this variable if you get Lisp errors
on Shell startup."
  :group 'cscomp
  :type 'integer)

(defcustom csharp-shell-exec-timeout 9
  "*Length of time in seconds to wait for the CscompShell to respond to commands
before giving up and signaling an error.  This isn't the total timeout; it's
the time to wait between chunks of response. "
  :group 'cscomp
  :type 'integer)

(defcustom csharp-shell-buffer-name "*CscompShell*"
  "Name of the Powershell buffer for C# completion support."
  :group 'cscomp
  :type 'string
  )

(defconst csharp-shell-prompt-string "CscompShell % "
  "The prompt string used for csharp-shell.  It is also used as a regex, so this string must contain no regex-sensitive sequences. Best to just leave it alone.")


(defvar cscomp-log-level 0
  "The current log level for C# completion operations.  0 = NONE, 1 = Info, 2 = VERBOSE, 3 = DEBUG. ")


(defun cscomp-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `cscomp-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (if (<= level cscomp-log-level)
      (let* ((msg (apply 'format text args)))
        (message "%s" msg)
        )))


(defun csharp-shell ()
  "Starts CsharpShell, which is an instance of Powershell that loads
a custom assembly dedicated to supporting C# code completion in emacs.
"
  (interactive)
  (csharp-shell--internal nil)
  )



(defun csharp-shell--internal (&optional display-buffer)
  (if (not (comint-check-proc csharp-shell-buffer-name))
      ;; then
      (let (version)
        (message "Starting CscompShell...")
        (setq version (csharp-shell--start))
        (message "CscompShell v%s is now running..." version)
       )
    ;; else
    (when display-buffer
      (message "CscompShell is already running."))
    )
  )


;; (defvar csharp-shell--reply nil
;;   "Internal use only.")



(defun csharp-shell-exec-and-maybe-eval-result (expr &optional eval-return)
  "Sends EXPR to the CscompShell.  EXPR could theoretically be
any valid Powershell command, but this fn is invoked from
csharp-completion, the EXPR is a call to static function in the
CscompUtilities.dll assembly.

If the shell is not already running, this function starts
it. Collects the text output from the shell.  If the optional
argument EVAL-RETURN is non-nil, this function returns the result
of evaluating the output as a Lisp expression. Otherwise, the
return value is the collected text.
"
  (let ((proc
         (or (get-buffer-process csharp-shell-buffer-name)
             (let (proc2)
               (csharp-shell--internal)
               (setq proc2 (get-buffer-process csharp-shell-buffer-name))
               proc2))))

    (if proc
        (let (reply tmp)

          (with-current-buffer csharp-shell-buffer-name
            (cscomp-log 2 "csharp-shell-exec: Sending: %s" expr)
            (setq reply
                  (powershell-invoke-command-silently proc expr csharp-shell-exec-timeout)))


          (if (or (null reply)
                  (string-match "// Error:" reply))
              (progn
                (cscomp-log 1
                          "csharp-shell-exec: CscompShell command error.\n  Expression: %s\n  Error: %s"
                          expr reply)
                (error "CscompShell eval error. See messages buffer for details.")))


          (if eval-return
              (if (and reply (not (string= reply "")))
                  (progn
                    (cscomp-log 2 "csharp-shell-exec: evaluating reply: '%s'" reply)

                    (setq tmp (read reply)) ;; get one s-exp
                    (if (not (eq tmp "CscompShell")) ;; means no response at all

                        (progn
                          (setq tmp (eval tmp))
                          (cscomp-log 2 "csharp-shell-exec: eval result(%s)" (prin1-to-string tmp)) ;; can be nil
                          tmp)
                      nil))

                ;; else
                (progn
                  (cscomp-log 1 "csharp-shell-exec: result is empty. Will not evaluate.")
                  nil))

            ;; else (no eval)
            (progn
              (cscomp-log 1 "csharp-shell-exec: no eval, reply: '%s'" reply)
              reply))))))






(defun csharp-shell-exec-and-eval-result (psh-statement)
  "Convenience function for evaluating Powershell statements
that return Lisp expressions as output. This function
invokes csharp-shell-exec with the evaluate-return option set to
t."
  (csharp-shell-exec-and-maybe-eval-result psh-statement t))




;; ;; dinoch - Thu, 20 May 2010  14:59
;; ;;
;; ;; TODO: now I cannot remember why this is here. Do I need to explicitly
;; ;; override the shell function?  Must check this.
;;
;; (defun shell (&optional buffer)
;;   "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
;; Interactively, a prefix arg means to prompt for BUFFER.
;; If BUFFER exists but shell process is not running, make new shell.
;; If BUFFER exists and shell process is running, just switch to BUFFER.
;; Program used comes from variable `explicit-shell-file-name',
;;  or (if that is nil) from the ESHELL environment variable,
;;  or (if that is nil) from `shell-file-name'.
;; If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
;; it is given as initial input (but this may be lost, due to a timing
;; error, if the shell discards input when it starts up).
;; The buffer is put in Shell mode, giving commands for sending input
;; and controlling the subjobs of the shell.  See `shell-mode'.
;; See also the variable `shell-prompt-pattern'.
;;
;; To specify a coding system for converting non-ASCII characters
;; in the input and output to the shell, use \\[universal-coding-system-argument]
;; before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
;; in the shell buffer, after you start the shell.
;; The default comes from `process-coding-system-alist' and
;; `default-process-coding-system'.
;;
;; The shell file name (sans directories) is used to make a symbol name
;; such as `explicit-csh-args'.  If that symbol is a variable,
;; its value is used as a list of arguments when invoking the shell.
;; Otherwise, one argument `-i' is passed to the shell.
;;
;; \(Type \\[describe-mode] in the shell buffer for a list of commands.)"
;;   (interactive
;;    (list
;;     (and current-prefix-arg
;;          (read-buffer "Shell buffer: "
;;                       (generate-new-buffer-name "*shell*")))))
;;   (setq buffer (get-buffer-create (or buffer "*shell*")))
;;   ;; Pop to buffer, so that the buffer's window will be correctly set
;;   ;; when we call comint (so that comint sets the COLUMNS env var properly).
;;   (pop-to-buffer buffer)
;;   (unless (comint-check-proc buffer)
;;     (let* ((prog (or explicit-shell-file-name
;;                      (getenv "ESHELL") shell-file-name))
;;            (name (file-name-nondirectory prog))
;;            (startfile (concat "~/.emacs_" name))
;;            (xargs-name (intern-soft (concat "explicit-" name "-args"))))
;;
;;       (unless (file-exists-p startfile)
;;         (setq startfile (concat "~/.emacs.d/init_" name ".sh")))
;;       (apply 'make-comint-in-buffer "shell" buffer prog
;;              (if (file-exists-p startfile) startfile)
;;              (if (and xargs-name (boundp xargs-name))
;;                  (symbol-value xargs-name)
;;                '("-i")))
;;       (shell-mode)))
;;   buffer)



(defun csharp-shell--start ()
  "Run a special instance of PowerShell in support of C# Completion, by invoking the `powershell' function.  The buffer containing the shell is named `csharp-shell-buffer-name'.
"

  (let* ((cscompshell-buffer (powershell
                            csharp-shell-buffer-name
                            csharp-shell-prompt-string))
         (proc (get-buffer-process cscompshell-buffer))
         (dll-location (concat
                        (or
                         csharp-shell-location-of-util-dll
                         "idontknow" )
                         "\\CscompUtilities.dll" ))
         result
         version)

    (if proc
        (progn
          ;; Don't need to call save-excursion here, because
          ;; powershell has already called pop-to-buffer .
          ;; The CscompShell is the current buffer, and all
          ;; the buffer-local variables are available.

          ;; load the CscompUtilities DLL .

          (setq result
                (powershell-invoke-command-silently
                 proc
                 (concat "[System.Reflection.Assembly]::LoadFrom('" dll-location "')")
                 6.5))

          (if (string-match "^Exception .*: \"\\(.+\\)\"" result)
              (let ((message (substring result (match-beginning 1) (match-end 1))))
              (error (concat "error: " message))))


          ;; get the version number
          (setq version
                (powershell-invoke-command-silently
                 proc
                 "[Ionic.Cscomp.Utilities]::Version()"
                 2.9))

          (if version
              (setq version (substring version 1 -1)))

          ;; If the user exits, we won't ask whether he wants to kill the CscompShell.
          (set-process-query-on-exit-flag proc nil)

          ;;(comint-simple-send proc "prompt\n") ;; shouldn't need this

          ;; Send an initial carriage-return.  The effect is to make the
          ;; prompt appear. I don't know why this is necessary here.
          ;; It's called in the powershell function, but somehow it has
          ;; no effect, when powershell is invoked from csharp-shell, so I
          ;; also call it here.
          (comint-send-input)
          (accept-process-output proc)

          ;; Remove the window for the shell.
          ;; shell.el automatically pops to the shell buffer, but we
          ;; don't want that in this case.  For Cscomp, the shell runs unseen,
          ;; in the background. User can pop to it, if he likes.
          (delete-window)

          )
      )

    ;; return the version of the CscompUtilities.dll
    version))


;; Set the default DLL location at load time,
;; if appropriate.
(or
 csharp-shell-location-of-util-dll
 (setq csharp-shell-location-of-util-dll
       (file-name-directory load-file-name)))


(provide 'csharp-shell)

;; End of csharp-shell.el
