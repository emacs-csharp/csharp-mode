;;; csharp-mode.el --- C# mode derived mode  -*- lexical-binding: t; -*-

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Jostein Kjønigsen <jostein@gmail.com>
;;            : Theodor Thornhill <theo@thornhill.no>
;; Created    : September 2020
;; Modified   : 2020
;; Version    : 0.10.0
;; Keywords   : c# languages oop mode
;; X-URL      : https://github.com/josteink/csharp-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(when (version< emacs-version "25.1")
  (require 'cl))
(require 'cc-mode)
(require 'cc-langs)

(eval-when-compile
  (require 'cc-fonts))

(require 'compile)

(eval-and-compile
  (c-add-language 'csharp-mode 'java-mode))

(defgroup csharp nil
  "Major mode for editing C# code."
  :group 'prog-mode)

(c-lang-defconst c-make-mode-syntax-table
  csharp `(lambda ()
            (let ((table (make-syntax-table)))
	      (c-populate-syntax-table table)
              (modify-syntax-entry ?@ "_" table)
	      table)))

(c-lang-defconst c-identifier-syntax-modifications
  csharp (append '((?@ . "w"))
	         (c-lang-const c-identifier-syntax-modifications)))

(c-lang-defconst c-symbol-start
  csharp (concat "[" c-alpha "_@]"))

(c-lang-defconst c-identifier-ops
  csharp '((left-assoc ".")))

(c-lang-defconst c-overloadable-operators
  csharp '("+" "-" "*" "/" "%" "&" "|" "^" "<<" ">>" "=="
	   "!=" ">" "<" ">=" "<="))

(c-lang-defconst c-multiline-string-start-char
  csharp ?@)

(c-lang-defconst c-type-prefix-kwds
  csharp '("class" "interface" "struct"))

(c-lang-defconst c-class-decl-kwds
  csharp '("class" "interface" "struct"))

;;; Keyword lists

(c-lang-defconst c-primitive-type-kwds
  csharp '("bool" "byte" "sbyte" "char" "decimal" "double" "float" "int" "uint"
	   "long" "ulong" "short" "ushort" "void" "object" "string" "var"))

(c-lang-defconst c-return-kwds
  csharp '("return"))

(c-lang-defconst c-typedef-kwds
  csharp nil)

(c-lang-defconst c-typeof-kwds
  c '("typeof" "is" "as"))

(c-lang-defconst c-type-modifier-prefix-kwds
  csharp '("volatile"))

(c-lang-defconst c-type-modifier-kwds
  csharp '("readonly" "new"))

(c-lang-defconst c-brace-list-decl-kwds
  csharp '("enum" "new"))

(c-lang-defconst c-recognize-post-brace-list-type-p
  csharp t)

(c-lang-defconst c-ref-list-kwds
  csharp nil)

(c-lang-defconst c-other-block-decl-kwds
  csharp '("namespace"))

(c-lang-defconst c-using-kwds
  csharp '("using"))

(c-lang-defconst c-equals-type-clause-kwds
  csharp '("using"))

(c-lang-defconst c-modifier-kwds
  csharp '("abstract" "default" "final" "native" "private" "protected"
	   "public" "partial" "internal" "readonly" "static" "event" "transient"
	   "volatile" "sealed" "ref" "out" "virtual" "implicit" "explicit"
	   "fixed" "override" "params" "async" "await" "extern" "unsafe"
           "get" "set" "this" "const" "delegate"))

(c-lang-defconst c-other-decl-kwds
  csharp '("using"))

(c-lang-defconst c-type-list-kwds
  csharp '("using"))

(c-lang-defconst c-other-kwds
  csharp '("select" "from" "where" "join" "in" "on" "equals" "into"
           "orderby" "descending" "group"))

(c-lang-defconst c-colon-type-list-kwds
  csharp '("class" "struct" "interface"))

(c-lang-defconst c-block-stmt-1-kwds
  csharp '("do" "else" "finally" "try"))

(c-lang-defconst c-block-stmt-1-2-kwds
  csharp '("try"))

(c-lang-defconst c-block-stmt-2-kwds
  csharp '("for" "if" "switch" "while" "catch" "foreach" "fixed" "checked"
	   "unchecked" "using" "lock"))

(c-lang-defconst c-simple-stmt-kwds
  csharp '("break" "continue" "goto" "throw" "return" "yield"))

(c-lang-defconst c-constant-kwds
  csharp  '("true" "false" "null" "value"))

(c-lang-defconst c-primary-expr-kwds
  csharp '("this" "base" "operator"))

(c-lang-defconst c-inexpr-class-kwds
  csharp nil)

(c-lang-defconst c-class-decl-kwds
  csharp '("class" "struct" "interface"))

(c-lang-defconst c-std-abbrev-keywords
  csharp (append (c-lang-const c-std-abbrev-keywords) '("catch" "finally")))

(c-lang-defconst c-decl-prefix-re
  csharp "\\([{}(;,<]+\\)")

(c-lang-defconst c-opt-type-suffix-key
  csharp (concat "\\(\\[" (c-lang-const c-simple-ws) "*\\]\\|\\.\\.\\.\\)"))

(c-lang-defconst c-recognize-typeless-decls
  csharp t)

(c-lang-defconst c-recognize-<>-arglists
  csharp t)

(c-lang-defconst c-opt-cpp-prefix
  csharp "\\s *#\\s *")

(c-lang-defconst c-opt-cpp-macro-define
  csharp (if (c-lang-const c-opt-cpp-prefix)
             "define"))

(c-lang-defconst c-cpp-message-directives
  csharp '("error" "warning" "region"))

(c-lang-defconst c-cpp-expr-directives
  csharp '("if" "elif"))

(c-lang-defconst c-other-op-syntax-tokens
  csharp  (append '("#")
	       (c-lang-const c-other-op-syntax-tokens)))

(c-lang-defconst c-line-comment-starter
  csharp "//")

(c-lang-defconst c-doc-comment-start-regexp
  csharp "///")

(defun csharp-at-vsemi-p (&optional pos)
  (if pos (goto-char pos))
  (or (and
       ;; Heuristics to find attributes
       (eq (char-before) ?\])
       (save-excursion
         (c-backward-sexp)
         (looking-at "\\[")))
      (and
       ;; Heuristics to find object initializers
       (save-excursion
         ;; Next non-whitespace character should be '{'
         (c-forward-syntactic-ws)
         (char-after ?{))
       (save-excursion
         ;; 'new' should be part of the line
         (beginning-of-line)
         (looking-at ".*new.*"))
       ;; Line should not already be terminated
       (not (eq (char-after) ?\;)))))

(c-lang-defconst c-at-vsemi-p-fn
  csharp 'csharp-at-vsemi-p)

(defun csharp-vsemi-status-unknown () t)

(c-lang-defconst c-vsemi-status-unknown-p-fn
  csharp 'csharp-vsemi-status-unknown-p)

(c-add-style "csharp"
             '("java"
               (c-basic-offset . 4)
               (c-comment-only-line-offset . (0 . 0))
               (c-offsets-alist . ((inline-open           . 0)
                                   (arglist-intro         . +)
                                   (arglist-close         . 0)
                                   (inexpr-class          . 0)
                                   (case-label            . +)
                                   (cpp-macro             . c-lineup-dont-change)
                                   (substatement-open     . 0)))))

(c-lang-defconst c-basic-matchers-before
  "Font lock matchers for basic keywords, labels, references and various
other easily recognizable things that should be fontified before generic
casts and declarations are fontified.  Used on level 2 and higher."
  csharp `(
           ;; Put warning face on unclosed strings
           ,@(if (version< emacs-version "27.0")
                 ;; Taken from 26.1 branch
                 `(,(c-make-font-lock-search-function
	             (concat ".\\(" c-string-limit-regexp "\\)")
	             '((c-font-lock-invalid-string))))
               `(("\\s|" 0 font-lock-warning-face t nil)))

           ;; Invalid single quotes
           c-font-lock-invalid-single-quotes

           ;; Fontify keyword constants
           ,@(when (c-lang-const c-constant-kwds)
	       (let ((re (c-make-keywords-re nil (c-lang-const c-constant-kwds))))
	         `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
			         1 c-constant-face-name)))))

           ;; Fontify all keywords except the primitive types.
           ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
	      1 font-lock-keyword-face)

           ,@(when (c-lang-const c-opt-identifier-concat-key)
	       `(,(c-make-font-lock-search-function
	           ;; Search for identifiers preceded by ".".  The anchored
	           ;; matcher takes it from there.
	           (concat (c-lang-const c-opt-identifier-concat-key)
	        	   (c-lang-const c-simple-ws) "*"
	        	   (concat "\\("
	        		   ;; "[" c-upper "]"
	        		   "[" (c-lang-const c-symbol-chars) "]*"
	        		   "\\|"
	        		   "\\)"))
	           `((let (id-end)
	               (goto-char (1+ (match-beginning 0)))
	               (while (and (eq (char-before) ?.)
	        		   (progn
	        		     (backward-char)
	        		     (c-backward-syntactic-ws)
	        		     (setq id-end (point))
	        		     (< (skip-chars-backward
	        			 ,(c-lang-const c-symbol-chars))
	        		        0))
	        		   (not (get-text-property (point) 'face)))
	        	 (c-put-font-lock-face (point) id-end
	        			       font-lock-variable-name-face)
	        	 (c-backward-syntactic-ws)))
	             nil
	             (goto-char (match-end 0))))))

           (eval . (list "\\(!\\)[^=]" 1 c-negation-char-face-name))
           ))

(c-lang-defconst c-basic-matchers-after
  csharp (append
          ;; merge with cc-mode defaults
          (c-lang-const c-basic-matchers-after)

          ;; function names
          `(("\\.\\([A-Za-z0-9_]+\\)[<(]" 1 font-lock-function-name-face t))
          ))

(defcustom csharp-font-lock-extra-types
  (list (concat "[" c-upper "]\\sw*[" c-lower "]\\sw"))
  (c-make-font-lock-extra-types-blurb "C#" "csharp-mode" (concat))
  :type 'c-extra-types-widget
  :group 'c)

(defconst csharp-font-lock-keywords-1 (c-lang-const c-matchers-1 csharp)
  "Minimal font locking for C# mode.")

(defconst csharp-font-lock-keywords-2 (c-lang-const c-matchers-2 csharp)
  "Fast normal font locking for C# mode.")

(defconst csharp-font-lock-keywords-3 (c-lang-const c-matchers-3 csharp)
  "Accurate normal font locking for C# mode.")

(defvar csharp-font-lock-keywords csharp-font-lock-keywords-3
  "Default expressions to highlight in C# mode.")

(defun csharp-font-lock-keywords-2 ()
  (c-compose-keywords-list csharp-font-lock-keywords-2))
(defun csharp-font-lock-keywords-3 ()
  (c-compose-keywords-list csharp-font-lock-keywords-3))
(defun csharp-font-lock-keywords ()
  (c-compose-keywords-list csharp-font-lock-keywords))

;;; Compilation support
;; When invoked by MSBuild, csc’s errors look like this:
;; subfolder\file.cs(6,18): error CS1006: Name of constructor must
;; match name of class [c:\Users\user\project.csproj]

(defun csharp--compilation-error-file-resolve ()
  "Resolve an msbuild error to a (filename . dirname) cons cell."
  ;; http://stackoverflow.com/a/18049590/429091
  (cons (match-string 1) (file-name-directory (match-string 4))))

(defconst csharp-compilation-re-msbuild-error
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?): "
   "error [[:alnum:]]+: [^\r\n]+\\[\\([^]\r\n]+\\)\\]$")
  "Regexp to match compilation error from msbuild.")

(defconst csharp-compilation-re-msbuild-warning
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?): "
   "warning [[:alnum:]]+: [^\r\n]+\\[\\([^]\r\n]+\\)\\]$")
  "Regexp to match compilation warning from msbuild.")

;; Notes on xbuild and devenv commonalities
;;
;; These regexes were tailored for xbuild, but apart from the concurrent
;; build-marker ("1>") they share exactly the same match-markers.
;;
;; If we don't exclude the match-markers explicitly, these regexes
;; will also be used to match for devenv as well, including the build-marker
;; in the file-name, causing the lookup to fail.
;;
;; So if we don't want devenv to fail, we actually need to handle it in our
;; xbuild-regexes, but then we automatically get devenv-support for free.

(defconst csharp-compilation-re-xbuild-error
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
   ;; handle weird devenv output format with 4 numbers, not 2 by having optional
   ;; extra capture-groups.
   "\\(?:,\\([0-9]+\\)\\)*): "
   "error [[:alnum:]]+: .+$")
  "Regexp to match compilation error from xbuild.")

(defconst csharp-compilation-re-xbuild-warning
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
   ;; handle weird devenv output format with 4 numbers, not 2 by having optional
   ;; extra capture-groups.
   "\\(?:,\\([0-9]+\\)\\)?*): "
   "warning [[:alnum:]]+: .+$")
  "Regexp to match compilation warning from xbuild.")

(eval-after-load 'compile
  (lambda ()
    (dolist
        (regexp
         `((xbuild-error
            ,csharp-compilation-re-xbuild-error
            1 2 3 2)
           (xbuild-warning
            ,csharp-compilation-re-xbuild-warning
            1 2 3 1)
           (msbuild-error
            ,csharp-compilation-re-msbuild-error
            csharp--compilation-error-file-resolve
            2
            3
            2
            nil
            (1 compilation-error-face)
            (4 compilation-error-face))
           (msbuild-warning
            ,csharp-compilation-re-msbuild-warning
            csharp--compilation-error-file-resolve
            2
            3
            1
            nil
            (1 compilation-warning-face)
            (4 compilation-warning-face))))
      (add-to-list 'compilation-error-regexp-alist-alist regexp)
      (add-to-list 'compilation-error-regexp-alist (car regexp)))))

;;; Monkey patch
(advice-add 'c-looking-at-inexpr-block
            :around 'csharp-looking-at-inexpr-block-hack)

(defun csharp-looking-at-inexpr-block-hack (orig-fun &rest args)
  (apply
   (if (eq major-mode 'csharp-mode)
       #'csharp-looking-at-inexpr-block
     orig-fun)
   args))

(defun csharp-looking-at-inexpr-block (lim containing-sexp &optional check-at-end)
  ;; Return non-nil if we're looking at the beginning of a block
  ;; inside an expression.  The value returned is actually a cons of
  ;; either 'inlambda, 'inexpr-statement or 'inexpr-class and the
  ;; position of the beginning of the construct.
  ;;
  ;; LIM limits the backward search.  CONTAINING-SEXP is the start
  ;; position of the closest containing list.  If it's nil, the
  ;; containing paren isn't used to decide whether we're inside an
  ;; expression or not.  If both LIM and CONTAINING-SEXP are used, LIM
  ;; needs to be farther back.
  ;;
  ;; If CHECK-AT-END is non-nil then extra checks at the end of the
  ;; brace block might be done.  It should only be used when the
  ;; construct can be assumed to be complete, i.e. when the original
  ;; starting position was further down than that.
  ;;
  ;; This function might do hidden buffer changes.

  (save-excursion
    (let ((res 'maybe) (passed-bracket-pairs 0) bracket-pos passed-paren
	  haskell-op-pos
	  (closest-lim (or containing-sexp lim (point-min)))
	  ;; Look at the character after point only as a last resort
	  ;; when we can't disambiguate.
	  (block-follows (and (eq (char-after) ?{) (point))))

      ;; Search for a C++11 "->" which suggests a lambda declaration.
      (when (and (c-major-mode-is 'c++-mode)
		 (setq haskell-op-pos
		       (save-excursion
			 (while
			     (progn
			       (c-syntactic-skip-backward "^;=}>" closest-lim t)
			       (and (eq (char-before) ?>)
				    (c-backward-token-2)
				    (not (looking-at c-haskell-op-re)))))
			 (and (looking-at c-haskell-op-re)
			      (point)))))
	(goto-char haskell-op-pos))

      (while (and (eq res 'maybe)
		  (progn (c-backward-syntactic-ws)
			 (> (point) closest-lim))
		  (not (bobp))
		  (progn (backward-char)
			 (or (looking-at "[\]\).]\\|\w\\|\\s_")
                             (looking-at ">")))
		  (c-safe (forward-char)
			  (goto-char (scan-sexps (point) -1))))

	(setq res
	      (if (looking-at c-keywords-regexp)
		  (let ((kw-sym (c-keyword-sym (match-string 1))))
		    (cond
		     ((and block-follows
			   (c-keyword-member kw-sym 'c-inexpr-class-kwds))
		      (and (not (eq passed-paren ?\[))
			   (or (not (looking-at c-class-key))
			       ;; If the class definition is at the start of
			       ;; a statement, we don't consider it an
			       ;; in-expression class.
			       (let ((prev (point)))
				 (while (and
					 (= (c-backward-token-2 1 nil closest-lim) 0)
					 (eq (char-syntax (char-after)) ?w))
				   (setq prev (point)))
				 (goto-char prev)
				 (not (c-at-statement-start-p)))
			       ;; Also, in Pike we treat it as an
			       ;; in-expression class if it's used in an
			       ;; object clone expression.
			       (save-excursion
				 (and check-at-end
				      (c-major-mode-is 'pike-mode)
				      (progn (goto-char block-follows)
					     (zerop (c-forward-token-2 1 t)))
				      (eq (char-after) ?\())))
			   (cons 'inexpr-class (point))))
		     ((c-keyword-member kw-sym 'c-paren-any-kwds) ; e.g. C++11 "throw" or "noexcept"
		      (setq passed-paren nil)
		      (setq passed-bracket-pairs 0)
		      (setq bracket-pos nil)
		      'maybe)
		     ((c-keyword-member kw-sym 'c-inexpr-block-kwds)
		      (when (not passed-paren)
			(cons 'inexpr-statement (point))))
		     ((c-keyword-member kw-sym 'c-lambda-kwds)
		      (when (or (not passed-paren)
				(eq passed-paren ?\())
			(cons 'inlambda (point))))
		     ((c-keyword-member kw-sym 'c-block-stmt-kwds)
		      nil)
		     (t
		      'maybe)))

		(if (looking-at "\\s(")
		    (if passed-paren
			(cond
			 ((and (eq passed-paren ?\[)
			       (eq (char-after) ?\[)
			       (not (eq (char-after (1+ (point))) ?\[))) ; C++ attribute.
			  ;; Accept several square bracket sexps for
			  ;; Java array initializations.
			  (setq passed-bracket-pairs (1+ passed-bracket-pairs))
			  'maybe)
			 ((and (eq passed-paren ?\()
			       (eq (char-after) ?\[)
			       (not (eq (char-after (1+ (point))) ?\[))
			       (eq passed-bracket-pairs 0))
			  ;; C++11 lambda function declaration
			  (setq passed-bracket-pairs 1)
			  (setq bracket-pos (point))
			  'maybe)
			 (t nil))
		      (when (not (looking-at "\\[\\["))
			(setq passed-paren (char-after))
			(when (eq passed-paren ?\[)
			  (setq passed-bracket-pairs 1)
			  (setq bracket-pos (point))))
		      'maybe)
		  'maybe)
		(if (and (c-major-mode-is 'csharp-mode)
                         (or (looking-at "([[:alnum:][:space:]_,]*)[ \t\n]*=>[ \t\n]*{")
			     (looking-at "[[:alnum:]_]+[ \t\n]*=>[ \t\n]*{")))
		    ;; If we are at a C# lambda header
		    (cons 'inexpr (point))))))
		
      (if (eq res 'maybe)
	  (cond
	   ((and (c-major-mode-is 'c++-mode)
		 block-follows
		 (eq passed-bracket-pairs 1)
		 (save-excursion
		   (goto-char bracket-pos)
		   (or (<= (point) (or lim (point-min)))
		       (progn
			 (c-backward-token-2 1 nil lim)
			 (and
			  (not (and (c-on-identifier)
				    (looking-at c-symbol-char-key)))
			  (not (looking-at c-opt-op-identifier-prefix)))))))
	    (cons 'inlambda bracket-pos))
	   ((and c-recognize-paren-inexpr-blocks
		 block-follows
		 containing-sexp
		 (eq (char-after containing-sexp) ?\())
	    (goto-char containing-sexp)
	    (if (or (save-excursion
		      (c-backward-syntactic-ws lim)
		      (while (and (eq (char-before) ?>)
				  (c-get-char-property (1- (point))
						       'syntax-table)
				  (c-go-list-backward nil lim))
			(c-backward-syntactic-ws lim))
		      (and (> (point) (or lim (point-min)))
			   (c-on-identifier)))
		    (and c-special-brace-lists
			 (c-looking-at-special-brace-list))
		    (and c-has-compound-literals
			 (save-excursion
			   (goto-char block-follows)
			   (not (c-looking-at-statement-block)))))
		nil
	      (cons 'inexpr-statement (point)))))

	res))))
;;; End of monkey patch

;;; Doc comments

(defconst csharpxml-font-lock-doc-comments
  ;; Most of this is taken from the javadoc example, however, we don't use the
  ;; '@foo' syntax, so I removed that. Supports the XML tags only
  `((,(concat "</?\\sw"			; XML tags.
	      "\\("
	      (concat "\\sw\\|\\s \\|[=\n\r*.:]\\|"
		      "\"[^\"]*\"\\|'[^']*'")
	      "\\)*>")
     0 ,c-doc-markup-face-name prepend nil)
    ("&\\(\\sw\\|[.:]\\)+;"		; XML entities.
     0 ,c-doc-markup-face-name prepend nil)
    (,(lambda (limit)
	(c-find-invalid-doc-markup "[<>&]\\|{@" limit))
     0 'font-lock-warning-face prepend nil)))

(defconst csharpxml-font-lock-keywords
  `((,(lambda (limit)
	(c-font-lock-doc-comments "///" limit
	  csharpxml-font-lock-doc-comments)))))

;;; End of doc comments

(defvar csharp-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table csharp))
  "Syntax table used in csharp-mode buffers.")

(defvar csharp-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in csharp-mode buffers.")

(easy-menu-define csharp-menu csharp-mode-map "C# Mode Commands"
  (cons "C#" (c-lang-const c-mode-menu csharp)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;; Custom variables
;;;###autoload
(defcustom csharp-mode-hook nil
  "*Hook called by `csharp-mode'."
  :type 'hook
  :group 'csharp)

;;;###autoload
(define-derived-mode csharp-mode prog-mode "C#"
  "Major mode for editing Csharp code.

Key bindings:
\\{csharp-mode-map}"
  :after-hook (c-update-modeline)
  (c-initialize-cc-mode t)
  (c-init-language-vars csharp-mode)
  (c-common-init 'csharp-mode)
  (easy-menu-add csharp-menu)
  (c-set-style "csharp")
  (setq-local c-doc-comment-style '((csharp-mode . csharpxml)))
  (c-run-mode-hooks 'c-mode-common-hook 'csharp-mode-hook))

(provide 'csharp-mode)

;;; csharp-mode.el ends here
