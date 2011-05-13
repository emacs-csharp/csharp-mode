;;; csharp-analysis.el -- C# source code analysis in emacs
;;
;; Author:     Dino Chiesa <dpchiesa@hotmail.com>
;; Maintainer: Dino Chiesa <dpchiesa@hotmail.com>
;; Created:    January 2011
;; Modified:   February 2011
;; Version:    0.2
;; Keywords:   c# languages oop mode
;; X-URL:      http://code.google.com/p/csharpmode/
;;
;;
;;; Commentary:
;;
;; This module provides sourcecode analysis to support the cscomp
;; (csharp-completion) module.
;;
;; ------------------------------------------------------------------
;;
;; The original csharp-completion package I wrote used the semantic
;; package from cedet. According to the description of the semantic
;; package, it is the perfect tool for the job. In practice, semantic is
;; complex, opaque, and brittle.  In general I find it a bloody
;; nightmare to use.  The documentation is absent or useless. Examining
;; the code reveals that it is disordered and haphazard.  In operation,
;; it treats C# namespaces as classes, and no matter what I tried I
;; couldn't get it to treat the namespace as an independent kind of
;; thing. It uses cute names for various features, in lieu of names that
;; actually convey meaning. There are many other problems. Because of
;; all that, it took me a great effort to understand enough to make it
;; somewhat usable for my purpose. But finally, I got there.
;;
;; But then! I upgraded from emacs v22 to 23.2, and semantic blew up.
;; Even the wisent file broke. Back to square one. I have asked for
;; support and assistance multiple times from the author and have never
;; received a response.
;;
;; Rather than once again invest time in working with semantic, I've
;; moved to the Ast magic from SharpDevelop -
;; ICSharpCode.NRefactory.dll.  The documentation for it is no better
;; than that for Semantic, but... it works as I expect it to.  It is
;; doscoverable, and it behaves rationally. It is complete.
;;
;; The .NET Dll is used via the CscompShell, which is a powershell that
;; loads the CscompUtilities.dll. One of the methods in that DLL returns
;; an abstract syntax tree, formatted as a lisp s-expression. It
;; constructs that s-exp using the NRefactory assembly.
;;
;; The way it works is to save the current buffer to a temporary file,
;; then run the syntax analysis on that temporary file, before deleting
;; the temp file.  Then, the code running in powershell loops through all the
;; source elements in the file, and emits a hierarchically arranged set
;; of lisp structures representing those elements.  Each element is
;; known as a "tag", borrowing from the language of the semantic
;; package.  A tag represents a namespace. the namespace tag has
;; children, which are "type" tags. Each type tag may have fields
;; methods and properties as children.  A method has a code block as a
;; child. And so on.
;;
;; This module exports functions that do things with that abstract
;; syntax tree - such as:
;;
;;   - list all of the namespaces in the current buffer
;;   - list all of the types in the given namespace
;;   - list all of the methods/fields/properties for the given type
;;   - list the in-scope local variables for the current point in the buffer
;;   - return the parent tag of a given tag
;;
;; All of this syntax analysis is performed in support of code
;; completion (intellisense).
;;
;; When the buffer changes, a flag is set that indicates that the AST is
;; out of date.  When the buffer stops changing, the analysis is run
;; again. Each analysis takes a second or so for a 1000-line module.
;;

(require 'cscomp-base)   ;; cscomp-log
(require 'csharp-shell)  ;; csharp-shell-invoke-shell-fn
(require 'cl) ;; first, second, dolist, etc

(defvar csharp-analysis-last-change-time nil
  "Time of last buffer change.")
(make-variable-buffer-local 'csharp-analysis-last-change-time)

(defvar csharp-analysis-syntax-tree nil
  "buffer-local result of the most recent syntactic analysis of
  the current buffer. It is a lisp s-expression. In general, this
  variable should not be referenced directly. Instead
  applications should call
  `csharp-analysis-get-analysis'")
(make-variable-buffer-local 'csharp-analysis-syntax-tree)

(defvar csharp-analysis-is-running nil
  "If t, source code analysis is running for the current C# buffer.")
(make-variable-buffer-local 'csharp-analysis-is-running)

(defvar csharp-analysis-no-changes-timeout 0.75
  "Time to wait after last change before starting syntax analysis. In the
timer event, if the last change is less that this amount of time ago, then
analysis is NOT performed. ")
(make-variable-buffer-local 'csharp-analysis-no-changes-timeout)

(defvar csharp-analysis-timer-interval 2
  "Interval in seconds for timer events. This is not the interval on
which analysis gets performed; it's the interval on which we check to
see if a new analysis is necessary. ")
(make-variable-buffer-local 'csharp-analysis-timer-interval)

(defvar csharp-analysis-syntax-tree-out-of-date t
  "a boolean indicating whether this module thinks the analysis
  of the sourcecode buffer is out of date. When this is true, and
  when the results of an analysis is requested via a call to
  `csharp-analysis-get-analysis', then a new analysis is
  performed.")
(make-variable-buffer-local 'csharp-analysis-syntax-tree-out-of-date)

(defvar csharp-analysis-timer nil
  "a timer object.")

(defvar csharp-analysis--query-cache nil
  "an alist. For internal use of the csharp-analysis module only.")

(defalias 'csharp-analysis-float-time
  (if (fboundp 'float-time)
      'float-time
    (if (featurep 'xemacs)
        (lambda ()
          (multiple-value-bind (s0 s1 s2) (values-list (current-time))
            (+ (* (float (ash 1 16)) s0) (float s1) (* 0.0000001 s2)))))))


(defun csharp-analysis-after-change-fn (start stop len)
  "invoked after the buffer changes. This fn simply records the
time of the last change.
"
      (cscomp-log 4 "after-change: recording change time")
      (setq csharp-analysis-last-change-time (csharp-analysis-float-time)))



(defun csharp-analysis-after-save-fn ()
  "a fn that gets invoked after the save of a buffer. If there is a local
  variable called `csharp-analysis-is-running', then cscomp immediately
  analyzes the sourcecode in the buffer."
  (if (local-variable-p 'csharp-analysis-is-running (current-buffer))
      (progn
        ;;(setq csharp-analysis-syntax-tree-out-of-date t)
        ;;(cscomp-log 3 "after-save: marking out of date")
        ;;(csharp-analysis-analyze-buffer)
        )))

(defun csharp-analysis-can-parse-file (file-name)
  "return t if the buffer is a C# source file."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (string-match "\\.cs$" file-name))



(defun csharp-analysis-timer-event (buffer)
  "This fn gets called by a timer, on a regular interval.  It
marks the buffer as out of date, in other words due for
sourcecode analysis, by setting `csharp-analysis-syntax-tree-out-of-date'
if appropriate.

A buffer is considered to be out of date when more than
`csharp-analysis-no-changes-timeout' seconds have elapsed since the last
change, and there have been other changes that have been made
during that interval.  This is checked by comparing the current
time to `csharp-analysis-last-change-time', the last change time that is
recorded in `csharp-analysis-after-change-fn'.

If the timeout period has not expired since the last change,
it indicates that the user is actively changing the buffer, eg
typing, and so there's no sense marking the buffer out of date at
the moment.

If the user later requests a sourcecode analysis (abstract syntax
tree, or AST) for the buffer by implicitly invoking
`csharp-analysis-get-analysis', if the sourcecode
analysis has previously been marked out of date, then cscomp
computes a new analysis.  If it is not marked out of date, then
cscomp returns the existing analysis.

When a buffer is initially loaded, the analysis is marked out of
date, so that it is always calculated upon the first request for
it.

"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (and csharp-analysis-last-change-time
               (not csharp-analysis-syntax-tree-out-of-date))
          (let ((elapsed (- (csharp-analysis-float-time) csharp-analysis-last-change-time)))
            (cscomp-log 4 "timer: elapsed since last change: %s"
                        (prin1-to-string elapsed))
            ;; see if the buffer has stopped changing
            (when (> elapsed csharp-analysis-no-changes-timeout)
              (setq csharp-analysis-syntax-tree-out-of-date t)))

        (cscomp-log 4 "timer: no change since previous analysis.")))))


(defun csharp-analysis-stop-timer ()
  (when csharp-analysis-timer
    (cancel-timer csharp-analysis-timer)
    (setq csharp-analysis-timer nil)))


(defun csharp-analysis-get-analysis ()
  "Returns the s-expression representing the sourcecode analysis of the
current buffer. If the current sourcecode analysis is out-of-date, then
the analysis is performed anew, and that new result is returned.  Several
events can trigger the out-of-date condition: addition of a newline (\n)
into the buffer; saving the buffer; other things. This module detects
these events in the appropriate emacs hooks - such as after-save-hook, or
the after-change-functions - and then sets the
`csharp-analysis-syntax-tree-out-of-date' variable to a non-nil value.

When this function is called and that variable is true, a new analysis is
performed. This may take some time, as it requires saving the buffer
contents to a temporary file, analyzing, then removing the file."

  (if (or csharp-analysis-syntax-tree-out-of-date
          (not csharp-analysis-syntax-tree))
      (csharp-analysis-analyze-buffer)
    csharp-analysis-syntax-tree))



(defun csharp-analysis-create-temp-filename (file-name)
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (let* ((prefix "cscomp")
         (temp-name (concat (file-name-sans-extension file-name)
                            "_" prefix
                            "."
                            (file-name-extension file-name))))

    (cscomp-log 3 "create-temp-filename: orig=%s temp=%s" file-name temp-name)
    temp-name))


(defun csharp-analysis-analyze-buffer ()
  "Actually do the analysis, by calling out to the AnalyzeSource method
available within the CscompShell. That method calls out to the NRefactory
library to perform the source analysis. The result is the lisp
s-expression that describes the syntax tree of the buffer. "

  ;; I'm not sure if timers create a multi-thread scenario or not.
  ;; in case they do, and in case there's an analysis already running,
  ;; let's just hack it and return the existing value.

  (if (and csharp-analysis-is-running
                 csharp-analysis-syntax-tree)
      (progn
        (cscomp-log 4 "csharp-analysis-analyze-buffer: analysis is already running.")
        ;;;
        csharp-analysis-syntax-tree)
    (let
      ((temp-file-name (csharp-analysis-create-temp-filename (buffer-file-name)))
       (delete-temp-file (lambda ()
            (when (file-exists-p temp-file-name)
              (delete-file temp-file-name)
              (cscomp-log 3 "deleted temp file %s" temp-file-name)))))

      ;; handle exceptions that occur during file creation,
      ;; saving, and analysis, via condition-case
      (condition-case exc1
          (progn
            (setq csharp-analysis-is-running t)
            ;; obtain mutex?
            (cscomp-log 1 "re-analyzing... ")
            (funcall delete-temp-file) ;; just in case it exists now


            ;; right here - maybe temporarily comment out the current
            ;; line?  maybe?

            ;; The problem is that NRefactory *quits* its analysis when
            ;; it encounters some classes of syntax errors.  which
            ;; errors?  I don't know. But in the case where a person is
            ;; asking for completions "right here", sometimes NRefactory
            ;; chokes on the incomplete line of code, which the user is
            ;; asking for completions on. As a result it doesn't emit a
            ;; CompilationUnit that contains any members following that
            ;; point.  If a variable gets initialized based on a field
            ;; that lies AFTER the current line in the buffer, then the
            ;; csharp-completion stuff won't work properly.

            ;; Just removing, or commenting out, the offending line
            ;; allows NRefactory to continue its analysis. That might be
            ;; the right thing to do here.  Have to figure out
            ;; why/when/how that should be done.

            ;; write the buffer to the temporary file
            (write-region nil nil temp-file-name nil 7)
            (cscomp-log 3 "wrote temp file %s" temp-file-name)
            (setq csharp-analysis-syntax-tree
                  (csharp-shell-invoke-shell-fn "GetAstForSourceFile" temp-file-name))
            (funcall delete-temp-file)

            (setq csharp-analysis-last-change-time nil
                  csharp-analysis-syntax-tree-out-of-date nil
                  csharp-analysis-is-running nil
                  csharp-analysis--query-cache nil) ;; clear cache

            ;; release mutex?
            csharp-analysis-syntax-tree)
        (progn
          (error "analysis failed.")
          (funcall delete-temp-file)
          (setq csharp-analysis-is-running nil)
          csharp-analysis-syntax-tree)))))


;;;###autoload
(define-minor-mode csharp-analysis-mode
  "A minor mode to do on-the-fly c# source code parsing and analysis.

When this function is called interactively, it toggles the minor
mode.

With arg, turn csharp-analysis mode on if and only if arg
is positive.

When this mode is on, emacs periodically analyzes the current
buffer and stores a representation of the abstract syntax
tree (AST) for the C# source code.  This AST is then used to
facilitate smart code completion, something like emacs'
autocomplete function, but smarter.  See
`cscomp-complete-at-point'.

"

  nil nil nil
  (cond
   ;; turn the mode ON.

   (csharp-analysis-mode
    (if (not (csharp-analysis-can-parse-file buffer-file-name))
        (cscomp-log 2 "cscomp cannot check syntax in buffer %s" (buffer-name))
      (add-hook 'after-change-functions 'csharp-analysis-after-change-fn nil t)
      (add-hook 'after-save-hook 'csharp-analysis-after-save-fn nil t)

      (add-hook 'kill-buffer-hook 'csharp-analysis-stop-timer nil t)

      ;; set a timer to fire on an interval
      (setq csharp-analysis-timer
          (run-at-time nil csharp-analysis-timer-interval
                       'csharp-analysis-timer-event
                       (current-buffer)))

      ;; do the first analysis?
      ;;(csharp-analysis-analyze-buffer)

      (cscomp-log 1 "csharp-analysis-mode: setting vars for initial conditions");
      (setq csharp-analysis-is-running nil)))

   ;; turn the mode OFF.
   (t
    (remove-hook 'after-change-functions 'csharp-analysis-after-change-fn t)
    (remove-hook 'after-save-hook 'csharp-analysis-after-save-fn t)
    (remove-hook 'kill-buffer-hook 'csharp-analysis-stop-timer t)
    (csharp-analysis-stop-timer)
    (setq csharp-analysis-is-running nil))))


;; ==================================================================
;; ==================================================================



(defun csharp-analysis-useful-taglist (ast)
  "returns the useful taglist from an AST.  The first, toplevel
AST is contains the string 'CompilationUnit as its car.  This fn
trims that, if it is present, and returns the list of children. If
it is not present, this fn just returhs the original taglist.
"
    (if (and
         (symbolp (first ast))
         (string= 'CompilationUnit (symbol-name (first ast))))
        (progn
          ;;(message "useful: %s" (prin1-to-string (cdadr ast)))
          (cdadr ast))
      ast))



;; -------------------------------------------------------
(defun csharp-analysis-get-toplevel-tags-from-ast (ast tag-type)
  "From the given AST, get toplevel tags of the given TAG-TYPE.
For example, pass TAG-TYPE of \"type\" to get all the toplevel
using clauses in an AST.  This would work for an AST representing
a namespace.

This fn does not consider nested elements.

Example:
    (csharp-analysis-get-toplevel-tags-from-ast myast \"type\")

See also:  `csharp-analysis-get-toplevel-tagnames-from-ast'.

"
  (when (not tag-type) (error "tag-type must be non-nil"))
  (let ((result (list)))
    (dolist (entry (csharp-analysis-useful-taglist ast))  ;; loop over the list
      (if (and
           (consp entry)
           (symbolp (first entry))
           (string= tag-type (csharp-analysis-tag-flavor entry)))
          (setq result (cons entry result))))
    (reverse result)))



(defun csharp-analysis-get-toplevel-tags (tag-type)
  "Get toplevel tags of the given TAG-TYPE from the AST for the
current buffer.  For example, pass TAG-TYPE of \"using\" to get
all the toplevel using clauses in an AST.  Does not get nested
elements.

Example:
    (csharp-analysis-get-toplevel-tags \"attribute\")

See also:  `csharp-analysis-get-toplevel-tagnames'.

"
  (interactive "stag type: ")
  (csharp-analysis-get-toplevel-tags-from-ast (csharp-analysis-get-analysis) tag-type))
;; -------------------------------------------------------



(defun csharp-analysis-nested-tags (node &optional include-params)
  "Get the nested tags (or nodes) for the given NODE (or tag).
In a namespace node, nested tags would be nodes representing
types, enumerated in an element called \"children\".  In a type,
the child nodes are also in a \"children\" node, and represent
methods, fields, and properties.  In a method, the children are
the expressions that define the method logic, and are available
in a node called \"block\".  Within a block, an \"if\" node has
children in \"then\" and \"else\" clauses.  A \"trycatch\" node
has children in the \"try\" clause, all \"catch\" clauses, and
the \"finally\" clause, if any.  And so on.

This fn is intended to support
`csharp-analysis-local-variables', which gets a list of local
variables.  To do that, the fn needs to know the current scope
tree.

"
  (when (not node) (return nil))

  (let* ((sname (symbol-name (first node)))
         (nested-tags
          (cond
           ((string= sname "if")
            (append
             (assoc 'then node)
             (csharp-analysis-get-toplevel-tags-from-ast node "elseif") ;; there may be multiple of these
             (assoc 'else node)))  ;; the else clause may be nil

           ((or
             (string= sname "foreach")
             (string= sname "for")
             (string= sname "block")
             (string= sname "then")
             (string= sname "else")
             (string= sname "elseif")
             (string= sname "finally")
             (string= sname "catch")
             (string= sname "try"))
            (cdr node))  ;; every child except the containing scope


           ((string= sname "trycatch")
            (append
             (csharp-analysis-get-toplevel-tags-from-ast node "try")
             (csharp-analysis-get-toplevel-tags-from-ast node "catch")
             (csharp-analysis-get-toplevel-tags-from-ast node "finally")))

           ((string= sname "property")
            (append
             (csharp-analysis-get-toplevel-tags-from-ast node "get") ;; may be nil
             (csharp-analysis-get-toplevel-tags-from-ast node "set"))) ;; may be nil

           (t
            (let (c)
              (setq c (assoc 'block node))
              (if c (list c)
                (setq c (assoc 'children node))
                (if c (cdr c)
                  nil))))
           ;;(csharp-analysis-get-toplevel-tags-from-ast node "block")
           ;;(csharp-analysis-get-toplevel-tags-from-ast node "children")

           )))

    (if include-params
        (let ((params (cadr (assoc 'params node))))
          (if params
              (setq nested-tags (append (list params) nested-tags)))))

    nested-tags))




(defun csharp-analysis-get-local-arguments-from-ast (ast)
  "From the given AST, get all the local arguments that are in scope at
the position defined by LINE and COL in the buffer described by the AST.

Within a method, this is the arguments to the method.

Within a property setter, this is the value argument.

Otherwise, nil.

"

  (let ((method (or
                 (csharp-analysis-get-enclosing-tag "method")
                 (csharp-analysis-get-enclosing-tag "ctor")
                 (csharp-analysis-get-enclosing-tag "set"))))

    (if method
         (csharp-analysis-method-params method))

    ))




(defun csharp-analysis-local-arguments ()
  "Gets all the local arguments that are in scope in the
current position in the buffer.

This includes arguments passed to the enclosing method, if any.
The result does not include local variables declared in the current method, or execution
block, or variables declared within any nested
curly-brace scopes that are active at the current buffer position.

An example output:

\((var \"args\" \"System.String[]\" (location (20 29) (20 38))))

This function prints its result if called interactively. This is for
diagnostic purposes only.

"
  (interactive)
  (let ((result
         (csharp-analysis-get-local-arguments-from-ast
          (csharp-analysis-useful-taglist (csharp-analysis-get-analysis)))))
    (if (called-interactively-p 'any)
        (message "result: %s" (prin1-to-string result)))
    result))




(defun csharp-analysis-get-local-variables-from-ast (ast line col &optional indent)
  "From the given AST, get all the local variables that are in scope at
the position defined by LINE and COL in the buffer described by the AST.

If the position is within a method, the result does not include
method parameters.  The result does include variables defined
within the scope of the method, as well as within any active
nested curly-brace scopes.

If the position is within some other execution scope, such as a
getter for a property, the result includes variables defined in
that scope as well as any active nested scopes.

The list excludes instance variables and method arguments that may
be available at the given position in the buffer.

See also: `csharp-analysis-local-arguments'

"
  (if (not indent)
    (setq indent ""))

  (let ((nodes ast)
        result)
    (while nodes
      (let ((node (car nodes)))
        (if (and (consp node) (symbolp (car node)))
            (let ((flavor (symbol-name (car node)))
                  (tag-name (csharp-analysis-tag-name node))
                  (location (cdr (assoc 'location node))))

              (cscomp-log 4 "%sconsidering tag (%s %s ...)"
                          indent flavor tag-name)

              (if (and location (csharp-analysis-location-contains-line-col location line col))
                  (progn
                    (cscomp-log 4 "%sCONTAINING SCOPE" indent)
                    (setq nodes nil) ;; this will be the last time through the loop
                    (if (or
                         (string= flavor "foreach")
                         (string= flavor "for")
                         (string= flavor "using")
                         (string= flavor "block")
                         (string= flavor "then")
                         (string= flavor "else")
                         (string= flavor "elseif")
                         (string= flavor "finally")
                         (string= flavor "catch")
                         (string= flavor "try"))

                        (progn
                          (cscomp-log 4 "%slooking for vars " indent)

                          (let ((all-vars
                                 (csharp-analysis-get-toplevel-tags-from-ast node "var"))
                                inner-result)

                            ;; include only those vars within the block that precede the current position
                            (if all-vars
                                (progn
                                  (cscomp-log 4 "%slooking at local vars..." indent)

                                  (while all-vars
                                    (let* ((one-var (car all-vars))
                                           (var-location (cdr (assoc 'location one-var)))
                                           (var-name (cadr one-var)))

                                      (cscomp-log 4 "%sconsidering var %s ..." indent var-name)

                                      ;; if the var decl precedes the current location, OR
                                      ;; if this is a catch variable, then include it in the list.
                                      (if (or
                                           (string= flavor "catch")
                                           (csharp-analysis-location-precedes-line-col var-location line col))
                                          (progn
                                            (cscomp-log 4 "%syes" indent)
                                            (setq inner-result (cons one-var inner-result)))))

                                    (setq all-vars (cdr all-vars)))

                                  (if inner-result
                                      (setq result (append result (reverse inner-result)))))

                              (cscomp-log 4 "%s  no local vars" indent))))

                      (cscomp-log 4 "%snot a var container" indent))

                    (let ((children (csharp-analysis-nested-tags node nil)))
                      (if children
                          (progn
                            (cscomp-log 4 "%srecursing..." indent)
                            (let ((r1 (csharp-analysis-get-local-variables-from-ast children line col (concat "  " indent))))
                              (if r1
                                  (setq result (append result r1)))))
                        (cscomp-log 4 "%sno children" indent))))

                (cscomp-log 4 "%snot contained within" indent)
                ))))

      (setq nodes (cdr nodes)))

    (cscomp-log 3 "%sresult '%s'" indent (prin1-to-string result))

    result))





(defun csharp-analysis-local-variables ()
  "Gets all the local variables that are in scope in the
current position in the buffer.

This includes variables declared in the current method, or execution
block, as well as variables declared within any nested
curly-brace scopes that are active at the current buffer position.

The list excludes instance variables that may be active at the
current point in the buffer, as well as parameters for the
enclosing method, if any.

An example output:

\((var \"args\" \"System.String[]\" (location (20 29) (20 38)))
  (var \"flavor\" \"System.Int32\" (location (20 44) (20 48)))
  (var \"z\" \"System.Int32\" (location (22 13) (22 22)))
  (var \"t\" \"var\" (location (52 13) (52 46)))
  (var \"i\" \"var\" (location (53 31) (53 32))))

This function prints its result if called interactively. This is for
diagnostic purposes only.

"
  (interactive)
  (cscomp-log 3 "ENTER local-variables")
  (let* ((line (line-number-at-pos))
         (col (current-column))
         (result
          (csharp-analysis-get-local-variables-from-ast
           (csharp-analysis-useful-taglist (csharp-analysis-get-analysis))
           line col)))
    (if (called-interactively-p 'any)
        (message "result: %s" (prin1-to-string result)))
    (cscomp-log 3 "local-variables: result %s" (prin1-to-string result))
    result))




;; -------------------------------------------------------
(defun csharp-analysis-get-toplevel-tagnames-from-ast (ast tag-type)
  "Get names of toplevel tags from the AST, of the given TAG-TYPE.
For example, pass TAG-TYPE of \"using\" to get all the toplevel
using clauses in an AST.

Retrieving using clauses would work with an AST representing a
compilation unit (full buffer) or a namespace.

Example:
    (csharp-analysis-get-toplevel-tagnames-from-ast myast \"using\")

See also:  `csharp-analysis-get-toplevel-tags-from-ast'.

"
  (mapcar
   'csharp-analysis-tag-name
   (csharp-analysis-get-toplevel-tags-from-ast ast tag-type)))





(defun csharp-analysis-get-toplevel-tagnames (tag-type)
  "Get the names of toplevel tags of the given TAG-TYPE, from the ast
for the current buffer.

For example, pass TAG-TYPE of \"using\" to get all the
toplevel using clauses in an AST.

Example:
    (csharp-analysis-get-toplevel-tagnames \"using\")

See also:  `csharp-analysis-get-toplevel-tags'.

"
  (interactive "stag type: ")
  (csharp-analysis-get-toplevel-tagnames-from-ast (csharp-analysis-get-analysis) tag-type))
;; -------------------------------------------------------





;; -------------------------------------------------------
(defun csharp-analysis-get-tags-from-ast (ast tag-type)
  "Get tags at any level from the given AST, of the given TAG-TYPE.
For example, pass TAG-TYPE of \"type\" to get all the types
defined in an AST.

Example:
    (csharp-analysis-get-tags-from-ast myast \"type\")

"
  (when (not tag-type) (error "tag-type must be non-nil"))

  (let ((result (list))
        (working-ast (csharp-analysis-useful-taglist ast)))

    (dolist (node working-ast)  ;; loop over the list
      (cond
       ;; It's possible that not all entries in the list are cons cells.
       ((consp node)
        (let ((node-flavor (first node))
              (node-name (second node))
              (children (csharp-analysis-nested-tags node t)))

          ;; (message "maybe: %s  [%s]"
          ;;          (prin1-to-string node-type)
          ;;          (prin1-to-string  node-name))

          (if (string= tag-type node-flavor)
            (setq result (cons node result)))

          (if (consp children)
            ;;(message "recurse")
            (setq result
                  (append
                   (reverse (csharp-analysis-get-tags-from-ast children tag-type))
                   result)))))
       (t nil)))
    (reverse result)))


(defun csharp-analysis-get-tags (tag-type)
  "Get tags of the given TAG-TYPE at any level from the ast for the current buffer.
For example, pass TAG-TYPE of \"type\" to get all the types
defined in a buffer.

Example:
  (csharp-analysis-get-tags \"type\")

"
  (interactive "stag type: ")
  (csharp-analysis-get-tags-from-ast (csharp-analysis-get-analysis) tag-type))
;; -------------------------------------------------------




;; -------------------------------------------------------
(defun csharp-analysis-get-tagnames-from-ast (ast tag-type)
  "Get names of tags from the AST, of the given TAG-TYPE.
For example, pass TAG-TYPE of \"type\" to get all the toplevel
types declared in an AST.

Example:
    (csharp-analysis-get-tagnames-from-ast myast \"type\")

See also:  `csharp-analysis-get-toplevel-tagnames-from-ast'.
See also:  `csharp-analysis-get-tagnames'.

"
  (mapcar
   'csharp-analysis-tag-name
   (csharp-analysis-get-tags-from-ast ast tag-type)))




(defun csharp-analysis-get-tagnames (tag-type)
  "Get names of all the tags of the given TAG-TYPE from the ast
for the current buffer. For example, pass TAG-TYPE of \"type\" to
get the names of all the types declared in an AST.

Example:
    (csharp-analysis-get-tagnames \"type\")

You could also use this to get all the using clauses that are
present in the buffer.

See also:  `csharp-analysis-get-tags'.


"
  (interactive "stag type: ")
  (csharp-analysis-get-tagnames-from-ast (csharp-analysis-get-analysis) tag-type))
;; -------------------------------------------------------




(defun csharp-analysis--find-parent-id-from-ast (taglist desired-id &optional indent)
"Helper fn."

  (when (not taglist) (return nil))

  (if (not indent)
      (setq indent ""))

  (cscomp-log 2 "%sfind-parent-id: looking for id: %d" indent desired-id)

  (let ((working-taglist (csharp-analysis-useful-taglist taglist)))

    ;; loop over the list, looking for a node with the given
    ;; name, considering only toplevel nodes.
    (dolist (node working-taglist)
      (if (consp node)
          (let ((this-flav (csharp-analysis-tag-flavor node))
                (this-name (csharp-analysis-tag-name node))
                (this-id (csharp-analysis-tag-id node)))
            (cscomp-log 2 "%sfind-parent-id: considering: (%s %s...(id %s))"
                        indent this-flav (prin1-to-string this-name)
                        (if (numberp this-id)
                            (prin1-to-string this-id)
                          "xx"))

            (if (numberp this-id)
                (progn
                  (if (= this-id desired-id)
                      (progn
                        (cscomp-log 2 "%sfind-parent-id: found %s" indent (prin1-to-string node))
                        (return working-taglist)))

                  (if (> this-id desired-id)
                      ;; recurse
                      (let ((children (csharp-analysis-nested-tags node t)))
                        (when children
                          (cscomp-log 2 "%sfind-parent-id: recurse" indent)
                          (let ((r1 (csharp-analysis--find-parent-id-from-ast
                                     children desired-id
                                     (concat indent "  "))))
                            (cond
                             ((numberp r1)
                              (return r1))
                             (r1
                              (return (csharp-analysis-tag-id node)))))))))))))))



(defun csharp-analysis-find-parent-tag-by-id-from-ast (taglist desired-id)
  "From the list of tags TAGLIST, returns the tag which is the parent
of the tag with id DESIRED-ID.

Returns nil if the parent cannot be found.

A tag represents one node in an abstract syntax table for a C#
buffer.  For example, a tag representing a using clause might
look like this:

  (import \"System\" nil
      (location (19 1) (19 14)) (id 2))

A tag representing a type declaration might look like this:

  (type \"AstRunner\"
     (modifier \"public\")
     (children ....)
     (location (37 5) (93 6)) (id 18))

"

  (cscomp-log 3 "find-parent-id: (%d)" desired-id)

  (if (eq desired-id 0) ;; special-case synthetic ID numbers
      nil
    (let* ((cache-key (format "parent-tag-by-id/%d" desired-id))
           (result (cadr (assoc cache-key csharp-analysis--query-cache))))

      (if result
          (progn
            (cscomp-log 3 "find-parent-id: cache hit, tag %s"
                        (prin1-to-string result))
            result)

        (let ((r1 (csharp-analysis--find-parent-id-from-ast taglist desired-id)))
          (cond
           ((numberp r1)
            (cscomp-log 2 "find-parent-id: found %d" r1)
            (let ((r2 (csharp-analysis-find-tag-by-id-from-ast taglist r1)))
              (cscomp-log 2 "find-parent-id: tag %s" (prin1-to-string r2))
              ;; insert into cache
              (setq csharp-analysis--query-cache
                    (append csharp-analysis--query-cache (list (list cache-key r2))))
              r2))

           ((consp r1)
            (let* ((r2 (csharp-analysis-tag-id r1))
                   (r3 (csharp-analysis-find-tag-by-id-from-ast taglist r2)))
              ;; insert into cache
              (setq csharp-analysis--query-cache
                    (append csharp-analysis--query-cache (list (list cache-key r3))))
              r3))

           (t nil)))))))






(defun csharp-analysis-find-tag-by-id-from-ast (taglist desired-id)
  "From the list of tags TAGLIST, returns the tag with id DESIRED-ID.

A tag represents one node in an abstract syntax table for a C#
buffer.  For example, a tag representing a using clause might
look like this:

  (import \"System\" nil
      (location (19 1) (19 14)) (id 2))

A tag representing a type declaration might look like this:

      (type \"AstRunner\"
        (modifier \"public\")
        (children ....)
        (location (37 5) (93 6)) (id 18))

"
  (when (not taglist) (return nil))

  (let ((working-taglist (csharp-analysis-useful-taglist taglist)))

    ;; loop over the list, looking for a node with the given
    ;; name, considering only toplevel nodes.
    (dolist (node working-taglist)
      (if (consp node)
          (let ((this-id (csharp-analysis-tag-id node)))
            (if (numberp this-id)
                (progn
                  (if (= this-id desired-id)
                      (return node))
                  (if (> this-id desired-id)
                      ;; recurse
                      (let ((children (csharp-analysis-nested-tags node t)))
                        (when children
                          (let ((r1 (csharp-analysis-find-tag-by-id-from-ast children desired-id)))
                            (when r1 (return r1)))))))))))))



(defun csharp-analysis-find-tag-by-id (id)
  "From the AST for the given buffer, get the tag with the given ID.
"
  (interactive "nTag #: ")
  (message "id: %d" id)
  (let ((result
         (csharp-analysis-find-tag-by-id-from-ast (csharp-analysis-get-analysis) id)))
    (if (called-interactively-p 'any)
        ;; If called interactively, show the result at
        ;; the bottom of the screen.
        (message "result: %s" (prin1-to-string result)))
    result))



(defun csharp-analysis-get-tag-parent-from-ast (taglist tag)
  "Returns the parent (container) tag of a given TAG.
Finds the parent of a tag.
"
  (error "not implemented"))



(defun csharp-analysis-get-tag-by-name (taglist name)
  "From the list of tags TAGLIST, returns the tag with name NAME.
The search is done breadth-first at the top-level, and then at depth.

A tag represents one node in an abstract syntax table for a C#
buffer.  For example, a tag representing a using clause might
look like this:

  (import \"System\" nil
      (location (19 1) (19 14)) (id 2))

A tag representing a type declaration might look like this:

      (type \"AstRunner\"
        (modifier \"public\")
        (children ....)
        (location (37 5) (93 6)) (id 18))

"
  (when (not name) (error "name must be non-nil"))

  (let ((working-taglist (csharp-analysis-useful-taglist taglist)))

    (or
     ;; loop over the list, looking for a node with the given
     ;; name, considering only toplevel nodes.
     (dolist (node working-taglist)
       (if (and (consp node)
                (stringp (second node))
                (string= (second node) name))
           (return node)))

     ;; loop over the list again, looking for the named node,
     ;; considering children nodes.
     (dolist (node working-taglist)
       (if (consp node)
           (let ((children (assoc 'children node))
                 r1)
             (when children
               (setq r1 (csharp-analysis-get-tag-by-name (cdr children) name))
               (when r1 (return r1)))))))))



(defun csharp-analysis-get-tag-by-type-and-name (taglist tag-type tag-name)
  "From TAGLIST, a list of tags, returns the tag with the given
TAG-TYPE and TAG-NAME.

A tag represents one node in an abstract syntax table for a C#
buffer.  For example, a tag representing a using clause might
look like this:

  (import \"System\" nil
      (location (19 1) (19 14)) (id 2))

A tag representing a type declaration might look like this:

      (type \"AstRunner\"
        (modifier \"public\")
        (children ....)
        (location (37 5) (93 6)) (id 18))

"
  (when (and (not tag-name)
             (not tag-type))
    (error "one of tag-name or tag-type must be non-nil"))

  (let ((working-taglist (csharp-analysis-useful-taglist taglist)))

    (or
     ;; loop over the list, looking for a node with the given
     ;; name and type, considering only toplevel nodes.
     (dolist (node working-taglist)
       (if (and (consp node)
                (symbolp (nth 0 node))
                (string= tag-type (symbol-name (nth 0 node)))
                (stringp (nth 1 node))
                (string= (nth 1 node) name))
           (return node)))

     ;; loop over the list again, looking for a matching node,
     ;; considering children nodes.
     (dolist (node working-taglist)
       (if (consp node)
           (let ((children (assoc 'children node))
                 r1)
             (when children
               (setq r1 (csharp-analysis-get-tag-by-name (cdr children) name))
               (when r1 (return r1)))))))))


(defun csharp-analysis-location-contains-line-col (location line col)
  "returns t if the given LOCATION (which implies a start and an end)
brackets the given LINE and COL pair.
"
  (let* ((start-loc (car location))
        (end-loc (cadr location))
        (start-line (car start-loc))
        (start-col (cadr start-loc))
        (end-line (car end-loc))
        (end-col (cadr end-loc)))
    (or
     (and
      (< start-line line)
      (> end-line line))
     (and
      (= start-line line)
      (<= start-col col))
     (and
      (= end-line line)
      (>= end-col col)))))


(defun csharp-analysis-location-precedes-line-col (location line col)
  "returns t if the given LOCATION ends before the given LINE and COL pair."
  (let* ((end-loc (cadr location))
         (end-line (car end-loc))
         (end-col (cadr end-loc)))
    (or
      (< end-line line)
     (and
      (= end-line line)
      (<= end-col col)))))



(defun csharp-analysis-get-enclosing-tag-from-ast (ast desired-tag-type line col namescope
                                                       &optional diag indent)
  "From the given AST, gets the narrowest enclosing tag of the
given DESIRED-TAG-TYPE. DESIRED-TAG-TYPE is a string, and can be
one of type, namespace, method, ctor, foreach, trycatch, etc.

\"Narrowest\" means, if searching for a DESIRED-TAG-TYPE of 'type, get
the smallest enclosing type. If the cursor is within a
nested (inner) class, return the tag for the inner class. If
there is a foreach inside a foreach, return the inner foreach.

LINE and COL define the position in the buffer described by AST,
for which to find the enclosing tag.

NAMESCOPE is the naming scope for the given AST. This allows this defun to be
recursive.

"
  (if (not diag)
      (setq diag "get-enclosing-tag"))

  (if (not indent)
      (setq indent ""))

  (if (not ast)
      (progn
        (message "%s: nothing to search within" diag)
        (return nil)))

  (cscomp-log 2 "%sENTER %s (%s)" indent diag desired-tag-type)

  (let ((nodes ast)
        result)

    ;; Loop over the list, looking for a node with start and end
    ;; locations that bracket the current location, considering only
    ;; toplevel nodes.  If found, then check for children.  If there are
    ;; children, push the name onto the namescope, and recurse.
    ;; children, recurse. If no children, then add the namescope to the
    ;; tag and return the tag. If the current toplevel tag does not
    ;; bracket the current location, the continue looping.
    (while (and nodes (not result))
      (let ((node (car nodes)))
        (if (and (consp node) (symbolp (first node)))
            (let ((location (cdr (assoc 'location node)))
                  (flavor (symbol-name (car node)))
                  (tag-name (csharp-analysis-tag-name node)))

              (cscomp-log 4 "%sconsidering tag (%s %s ...)"
                          indent flavor tag-name)


              (if (and location (csharp-analysis-location-contains-line-col location line col))
                  (progn
                    (cscomp-log 4 "%sCONTAINING SCOPE..." indent)
                    (setq nodes nil) ;; terminate outer loop

                    (let ((children (csharp-analysis-nested-tags node t))
                          (matched-tag (string= desired-tag-type (symbol-name (car node)))))

                      (setq result
                            (if children
                                (let ((ns2 (if (string= namescope "")
                                               tag-name
                                             (concat namescope "." tag-name)))
                                       r1)
                                  (cscomp-log 4 "%srecursing..." indent)

                                  (setq r1
                                        (csharp-analysis-get-enclosing-tag-from-ast
                                         children desired-tag-type line col ns2
                                         diag
                                         (concat indent "  ")))
                                  (or r1
                                      (and matched-tag
                                           (append node (list (list 'namescope (format "%s" namescope)))))))

                              ;; no children
                              (and matched-tag
                                   (append node (list (list 'namescope (format "%s" namescope)))))))))))))

      (setq nodes (cdr nodes)))

    result))





(defun csharp-analysis-get-enclosing-tag (tag-type &optional diag)
  "Gets the current tag of the provided TAG-TYPE for the current
position in the buffer.

The TAG-TYPE can be one of: type, method, namespace, etc.

"
  (interactive "stag type: ")

  (if (not diag)
      (setq diag "get-enclosing-tag"))

  (cscomp-log 3 "%s (%s)" diag tag-type)

  (let* ((line (line-number-at-pos))
         (col (current-column))
         (cache-key (format "enclosing-tag/%s-%d-%d" tag-type line col))
         (result (cadr (assoc cache-key csharp-analysis--query-cache))))

    (if result
        (cscomp-log 3 "%s cache hit" diag)
      (let ((nodes (csharp-analysis-useful-taglist (csharp-analysis-get-analysis))))
        (setq result
              (csharp-analysis-get-enclosing-tag-from-ast nodes tag-type line col "" diag))
        ;; insert into the cache
        (setq csharp-analysis--query-cache
              (append csharp-analysis--query-cache (list (list cache-key result))))))


    (if (called-interactively-p 'any)
        (progn
      ;; If called interactively, show the result at
      ;; the bottom of the screen.
      (message "result: %s" (prin1-to-string result))
      (if result
          (message "result: %s (%s)"
                   (csharp-analysis-tag-name result)
                   (csharp-analysis-tag-flavor result)))))
    (cscomp-log 3 "%s: %s" diag (prin1-to-string result))
    result))



(defun csharp-analysis-instance-variables ()
  "Return the list of instance variables (fields or properties)
that are in scope.

The return value is a list of nodes from the abstract syntax
tree, with any hierarchy flattened. The list looks like:

  ((field \"_index\" \"System.Int32\" (modifier private) (location (14 9) (14 28)) (id 5))
   (field \"_Value\" \"System.Int32\" (modifier private) (location (78 9) (78 28)) (id 47))
   (property \"Verbose\" \"System.Boolean\" (modifier public) (get) (set) (location (9 9) (12 10)) (id 4))
   (property \"Value\" \"System.Int32\" (modifier public) (get ...) (set ...) (location (79 9) (92 10)) (id 57))
   (property \"Seven\" \"System.Int32\" (modifier public) (get) (set (modifier private)) (location (94 9) (98 10)) (id 58)))

see also:  `csharp-analysis-local-variables'

"
  (interactive)
  (let* ((class (csharp-analysis-get-enclosing-tag 'type))
         (children (cdr (assoc 'children class)))
         (all-vars
          (append
           (csharp-analysis-get-toplevel-tags-from-ast children "field")
           (csharp-analysis-get-toplevel-tags-from-ast children "property"))))

    (cscomp-log 3 "instance-variables: found instance vars: '%s'" all-vars)
    (if (called-interactively-p 'any)
        (message "result: %s"
                   (mapcar
                    '(lambda (item) (list (car item) (cadr item) (caddr item)))
                    all-vars)))
    all-vars))


(defun csharp-analysis-instance-members ()
  "Return the list of instance members in scope, in a C# module.
Members include constructors, properties, fields, and methods.

The return value is a list of AST nodes.  It looks like:

\( (ctor \"falafel\" (modifier \"public\") (params nil)
        (block (location (7 27) (7 29)))
        (location (7 16) (7 29)) (id 18))
  (property \"Verbose\" \"System.Boolean\" (modifier public) (location (8 9) (8 28)) (id 29))
  (field \"_index\" \"System.Int32\" (modifier \"private\") (location (13 9) (13 28)) (id 32))
  (method \"Method1\" \"System.Void\" (modifier \"public\") (params (var \"args\" \"System.String[]\"))
          (block ... (location (16 9) (44 10)))
          (location (15 9) (44 10))
          (id 51)))

"
  (csharp-analysis-class-members))



;; ;;
;; ;; Examples
;; ;;
;;
;; (message "toplevel usings: %s"
;;          (prin1-to-string
;;           (csharp-analysis-get-toplevel-tagnames "using")))
;;
;; ;; expect nil
;; (message "toplevel types: %s"
;;          (prin1-to-string
;;           (csharp-analysis-get-toplevel-tagnames "type")))
;;
;; (message "all usings: %s"
;;          (prin1-to-string
;;           (cscomp-get-tagnames-from-ast ralph "using")))
;;
;; (message "namespaces: %s"
;;          (prin1-to-string
;;           (csharp-analysis-get-toplevel-tagnames "namespace")))
;;
;; (message "namespaces: %s"
;;          (prin1-to-string
;;           (csharp-analysis-get-toplevel-tags "namespace")))
;;
;; (message "one namespace: %s"
;;          (prin1-to-string
;;           (csharp-analysis-get-toplevel-tag "namespace" "Ionic.ToolsAndTests")))
;;
;; (message "types (all): %s"
;;          (prin1-to-string
;;           (cscomp-get-tags "type")))
;;
;; (message "names of types (all): %s"
;;          (prin1-to-string
;;           (cscomp-get-tagnames "type")))
;;
;;
;; ;; expected: nil
;; ;; there is no type element at the top level
;; (message "one type: %s"
;;          (prin1-to-string
;;           (csharp-analysis-get-toplevel-tag "type" "AstSexpression")))
;;
;;
;;
;; (message "named type: %s"
;;          (prin1-to-string
;;           (cscomp-get-tag-by-name (cscomp-get-tags "type") "AstRunner")))
;; (message "named type: %s"
;;           (prin1-to-string
;;            (cscomp-get-tag-by-name ralph "AstRunner")))
;;
;;
;; (message "named field: %s"
;;          (prin1-to-string
;;           (cscomp-get-tag-by-name (cscomp-get-tags "field") "depth")))
;;
;;
;; (message "methods: %s"
;;          (prin1-to-string
;;           (csharp-analysis-get-toplevel-tagnames
;;            (first
;;             (cscomp-get-tags
;;              (first
;;               (csharp-analysis-get-toplevel-tags "namespace"))
;;              "type"))
;;            "method" )))
;;
;;
;; (message "fields: %s"
;;          (prin1-to-string
;;           (csharp-analysis-get-toplevel-tagnames
;;            (first                           ;; use the first type defined in the file
;;             (csharp-analysis-get-toplevel-tags            ;; get all types
;;              (first
;;               (csharp-analysis-get-toplevel-tags "namespace"))
;;              "type"))
;;            "field" )))
;;
;; (message "my type: %s"
;;          (prin1-to-string
;;           (cscomp-get-tag-from-ast
;;            (first (csharp-analysis-get-toplevel-tags "namespace"))
;;            "type" "AstRunner")))
;;
;; (message "one method: %s"
;;          (prin1-to-string
;;           (cscomp-get-tag-from-ast
;;            (csharp-analysis-get-toplevel-tags "namespace")
;;           "method" "Run")))
;;
;;
;; (message "one field: %s"
;;          (prin1-to-string
;;           (cscomp-get-tag
;;            (cscomp-get-tags
;;             (first                           ;; use the first type defined in the file
;;              (csharp-analysis-get-toplevel-tags            ;; get all types
;;               (first
;;                (csharp-analysis-get-toplevel-tags "namespace"))
;;               "type"))
;;             "field" )
;;            "field" "depth")))



;; (dolist (entry ralph)  ;; loop over the list
;;   (cond
;;    ((consp entry)
;;     (case (first entry)   ;; get the car of the entry
;;       (allocate (plot-allocation (second entry)))
;;       (free (plot-free (second entry)))))
;;    (t
;;     (message "entry: %s" entry))))
;;
;;
;;

(defun csharp-analysis-tag-name (tag)
  "get the name from the given TAG. The result is a string, the name
of the method, class, property, field, or whatever."
  (when (not tag) (error "no tag available."))
  (when (not (consp tag)) (error "csharp-analysis-tag-name: the tag provided is not valid."))
  (if (stringp (nth 1 tag))
      (nth 1 tag)
    "--"))

(defun csharp-analysis-tag-flavor (tag)
  "get the flavor of the given TAG. The result is a string like
ctor, method, property, field, namespace, and so on."
  (when (not tag) (error "no tag available."))
  (when (not (consp tag)) (error "csharp-analysis-tag-flavor: the tag provided is not valid."))
  (symbol-name (nth 0 tag)))


(defun csharp-analysis-tag-type (tag)
  "get the type of the given TAG.  The result is a string like
System.Int32, bool, and so on."
  (and (stringp (nth 2 tag))
       (nth 2 tag)))

(defun csharp-analysis-tag-modifiers (tag)
  "get the modifier clause attached to the given TAG, if any.  The
result is a string like \"public static\" or \"private\".  The result
is nil if there is no modifier.
"
  (nth 1 (assoc 'modifier tag)))

(defun csharp-analysis-tag-location (tag)
  "get the location clause attached to the given TAG, if any.  The
result is a list like \(location (START-LINE . START-COL) (END-LINE . END-COL)) .
The result is nil if there is no modifier.
"
  (assoc 'location tag))


(defun csharp-analysis-tag-id (tag)
  "get the id attached to the given TAG, if any.  The
result is an integer, or nil.

"
  (nth 1 (assoc 'id tag)))



(defun csharp-analysis-method-params (tag)
  "get the parameters attached to the given TAG, if any.  The tag
must be a method to get a meaningful result.  The result is a
list describing the parameters for the method, with each entry a
list of (NAME TYPE).  The result is nil if there are no parameters, or
if the tag is not a method.
"
  (cdr (assoc 'params tag)))


(defun csharp-analysis-current-class ()
  "Gets the tag for the class containing point.
"
  (csharp-analysis-get-enclosing-tag 'type "current-class"))


(defun csharp-analysis-class-members (tag)
  "Gets the members of the class described by the TAG.
If TAG is nil, then this function gets the narrowest type
containing point, and gets all the members of that.

"
  (if (null tag) (setq tag (csharp-analysis-current-class)))
  (if (string= (csharp-analysis-tag-flavor tag) "type")
      (cdr (assoc 'children tag))
    nil))


(provide 'csharp-analysis)

;;; end of csharp-analysis.el
