;;; csharp-mode.el --- C# mode derived mode  -*- lexical-binding: t; -*-

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Jostein Kjønigsen <jostein@gmail.com>
;;            : Theodor Thornhill <theo@thornhill.no>
;; Created    : September 2020
;; Modified   : 2020
;; Version    : 0.10.0
;; Keywords   : c# languages oop mode
;; X-URL      : https://github.com/josteink/csharp-mode
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.12.1") (seq "2.21"))

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
(require 'cl-lib)
(require 'seq)
(require 'tree-sitter)
(require 'tree-sitter-hl)

(when (version< emacs-version "25.1")
  (require 'cl))
(require 'cc-mode)
(require 'cc-langs)

(eval-when-compile
  (require 'cc-fonts))

(require 'compile)

(defgroup csharp nil
  "Major mode for editing C# code."
  :group 'prog-mode)


(eval-and-compile
  (defconst csharp--regex-identifier
    "[A-Za-z][A-Za-z0-9_]*"
    "Regex describing an dentifier in C#.")

  (defconst csharp--regex-identifier-matcher
    (concat "\\(" csharp--regex-identifier "\\)")
    "Regex matching an identifier in C#.")

  (defconst csharp--regex-type-name
    "[A-Z][A-Za-z0-9_]*"
    "Regex describing a type identifier in C#.")

  (defconst csharp--regex-type-name-matcher
    (concat "\\(" csharp--regex-type-name "\\)")
    "Regex matching a type identifier in C#.")

  (defconst csharp--regex-using-or-namespace
    (concat "^using" "\\|" "namespace"
            "\\s *"
            csharp--regex-type-name-matcher)
    "Regex matching identifiers after a using or namespace
    declaration."))

(eval-and-compile
  (c-add-language 'csharp-mode 'java-mode))

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

(c-lang-defconst c-opt-type-suffix-key
  csharp (concat "\\(\\[" (c-lang-const c-simple-ws) "*\\]\\|\\?\\)"))

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

(c-lang-defconst c-other-decl-kwds
  csharp nil)

(c-lang-defconst c-type-list-kwds
  csharp nil)

(c-lang-defconst c-other-block-decl-kwds
  csharp nil)

(c-lang-defconst c-return-kwds
  csharp '("return"))

(c-lang-defconst c-typedef-kwds
  csharp nil)

(c-lang-defconst c-typeof-kwds
  csharp '("typeof" "is" "as"))

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

(c-lang-defconst c-using-kwds
  csharp '("using"))

(c-lang-defconst c-equals-type-clause-kwds
  csharp '("using"))

(defun csharp-at-vsemi-p (&optional pos)
  (if pos (goto-char pos))
  (save-excursion
    (beginning-of-line)
    (c-forward-syntactic-ws)
    (looking-at "using\\s *(")))

(c-lang-defconst c-at-vsemi-p-fn
  csharp 'csharp-at-vsemi-p)

(defun csharp-vsemi-status-unknown () t)

(c-lang-defconst c-vsemi-status-unknown-p-fn
  csharp 'csharp-vsemi-status-unknown-p)


(c-lang-defconst c-modifier-kwds
  csharp '("abstract" "default" "final" "native" "private" "protected"
	   "public" "partial" "internal" "readonly" "static" "event" "transient"
	   "volatile" "sealed" "ref" "out" "virtual" "implicit" "explicit"
	   "fixed" "override" "params" "async" "await" "extern" "unsafe"
           "get" "set" "this" "const" "delegate"))

(c-lang-defconst c-other-kwds
  csharp '("select" "from" "where" "join" "in" "on" "equals" "into"
           "orderby" "ascending" "descending" "group" "when"
           "let" "by" "namespace"))

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

(eval-and-compile
  (unless (assoc 'csharp-mode c-default-style)
    (setq c-default-style
          (cons '(csharp-mode . "csharp")
                c-default-style))))

(defun csharp--color-forwards (font-lock-face)
  (let (id-beginning)
    (goto-char (match-beginning 0))
    (forward-word)
    (while (and (not (or (eq (char-after) ?\;)
                         (eq (char-after) ?\{)))
	        (progn
	          (forward-char)
	          (c-forward-syntactic-ws)
	          (setq id-beginning (point))
	          (> (skip-chars-forward
	              (c-lang-const c-symbol-chars))
	             0))
	        (not (get-text-property (point) 'face)))
      (c-put-font-lock-face id-beginning (point) font-lock-face)
      (c-forward-syntactic-ws))))

(c-lang-defconst c-basic-matchers-before
  csharp `(
           ;; Warning face on unclosed strings
           ,@(if (version< emacs-version "27.0")
                 ;; Taken from 26.1 branch
                 `(,(c-make-font-lock-search-function
	             (concat ".\\(" c-string-limit-regexp "\\)")
	             '((c-font-lock-invalid-string))))
               `(("\\s|" 0 font-lock-warning-face t nil)))

           ;; Invalid single quotes
           c-font-lock-invalid-single-quotes

           ;; Keyword constants
           ,@(when (c-lang-const c-constant-kwds)
	       (let ((re (c-make-keywords-re nil (c-lang-const c-constant-kwds))))
	         `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
			         1 c-constant-face-name)))))

           ;; Keywords except the primitive types.
           ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
	      1 font-lock-keyword-face)

           ;; Chained identifiers in using/namespace statements
           ,`(,(c-make-font-lock-search-function
                csharp--regex-using-or-namespace
	        `((csharp--color-forwards font-lock-variable-name-face)
	          nil
	          (goto-char (match-end 0)))))


           ;; Negation character
           (eval . (list "\\(!\\)[^=]" 1 c-negation-char-face-name))

           ;; Types after 'new'
           (eval . (list (concat "\\<new\\> *" csharp--regex-type-name-matcher)
                         1 font-lock-type-face))

           ;; Single identifier in attribute
           (eval . (list (concat "\\[" csharp--regex-type-name-matcher "\\][^;]")
                         1 font-lock-variable-name-face t))

           ;; Function names
           (eval . (list "\\([A-Za-z0-9_]+\\)\\(<[a-zA-Z0-9, ]+>\\)?("
                         1 font-lock-function-name-face))

           ;; Nameof
           (eval . (list (concat "\\(\\<nameof\\>\\) *(")
                         1 font-lock-function-name-face))

           (eval . (list (concat "\\<nameof\\> *( *"
                                 csharp--regex-identifier-matcher
                                 " *) *")
                         1 font-lock-variable-name-face))

           ;; Catch statements with type only
           (eval . (list (concat "\\<catch\\> *( *"
                                 csharp--regex-type-name-matcher
                                 " *) *")
                         1 font-lock-type-face))
           ))

(c-lang-defconst c-basic-matchers-after
  csharp (append
          ;; Merge with cc-mode defaults - enables us to add more later
          (c-lang-const c-basic-matchers-after)))

(defcustom csharp-codedoc-tag-face 'c-doc-markup-face-name
  "Face to be used on the codedoc docstring tags.
Should be one of the font lock faces, such as
`font-lock-variable-name-face' and friends.
Needs to be set before `csharp-mode' is loaded, because of
compilation and evaluation time conflicts."
  :type 'symbol
  :group 'csharp)

(defcustom csharp-font-lock-extra-types
  (list csharp--regex-type-name)
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


;;; Doc comments

(defconst codedoc-font-lock-doc-comments
  ;; Most of this is taken from the javadoc example, however, we don't use the
  ;; '@foo' syntax, so I removed that. Supports the XML tags only
  `((,(concat "</?\\sw"			; XML tags.
	      "\\("
	      (concat "\\sw\\|\\s \\|[=\n\r*.:]\\|"
		      "\"[^\"]*\"\\|'[^']*'")
	      "\\)*/?>")
     0 ,csharp-codedoc-tag-face prepend nil)
    ;; ("\\([a-zA-Z0-9_]+\\)=" 0 font-lock-variable-name-face prepend nil)
    ;; ("\".*\"" 0 font-lock-string-face prepend nil)
    ("&\\(\\sw\\|[.:]\\)+;"		; XML entities.
     0 ,csharp-codedoc-tag-face prepend nil)))

(defconst codedoc-font-lock-keywords
  `((,(lambda (limit)
	(c-font-lock-doc-comments "///" limit
	  codedoc-font-lock-doc-comments)))))

;;; End of doc comments

;;; Adding syntax constructs

(advice-add 'c-looking-at-inexpr-block
            :around 'csharp-looking-at-inexpr-block)

(defun csharp-looking-at-inexpr-block (orig-fun &rest args)
  (let ((res (csharp-at-lambda-header)))
    (if res
        res
      (apply orig-fun args))))

(defun csharp-at-lambda-header ()
  (save-excursion
    (c-backward-syntactic-ws)
    (unless (bobp)
      (backward-char)
      (c-safe (goto-char (scan-sexps (point) -1)))
      (when (or (looking-at "([[:alnum:][:space:]_,]*)[ \t\n]*=>[ \t\n]*{")
	        (looking-at "[[:alnum:]_]+[ \t\n]*=>[ \t\n]*{"))
        ;; If we are at a C# lambda header
        (cons 'inexpr (point))))))

(advice-add 'c-guess-basic-syntax
            :around 'csharp-guess-basic-syntax)

(defun csharp-guess-basic-syntax (orig-fun &rest args)
  (cond
   (;; Attributes
    (save-excursion
      (goto-char (c-point 'iopl))
      (and
       (eq (char-after) ?\[)
       (save-excursion
         (c-go-list-forward)
         (and (eq (char-before) ?\])
              (not (eq (char-after) ?\;))))))
    `((annotation-top-cont ,(c-point 'iopl))))

   ((and
     ;; Heuristics to find object initializers
     (save-excursion
       ;; Next non-whitespace character should be '{'
       (goto-char (c-point 'boi))
       (eq (char-after) ?{))
     (save-excursion
       ;; 'new' should be part of the line
       (goto-char (c-point 'iopl))
       (looking-at ".*\\s *new\\s *.*"))
     ;; Line should not already be terminated
     (save-excursion
       (goto-char (c-point 'eopl))
       (or (not (eq (char-before) ?\;))
           (not (eq (char-before) ?\{)))))
    (if (save-excursion
          ;; if we have a hanging brace on line before
          (goto-char (c-point 'eopl))
          (eq (char-before) ?\{))
        `((brace-list-intro ,(c-point 'iopl)))
      `((block-open) (statement ,(c-point 'iopl)))))
   (t
    (apply orig-fun args))))

;;; End of new syntax constructs



;;; Fix for strings on version 27.1

(when (version= emacs-version "27.1")
  ;; See:
  ;; https://github.com/josteink/csharp-mode/issues/175
  ;; https://github.com/josteink/csharp-mode/issues/151
  ;; for the full story.
  (defun c-pps-to-string-delim (end)
    (let* ((start (point))
	   (no-st-s `(0 nil nil ?\" nil nil 0 nil ,start nil nil))
	   (st-s `(0 nil nil t nil nil 0 nil ,start nil nil))
	   no-st-pos st-pos
	   )
      (parse-partial-sexp start end nil nil no-st-s 'syntax-table)
      (setq no-st-pos (point))
      (goto-char start)
      (while (progn
	       (parse-partial-sexp (point) end nil nil st-s 'syntax-table)
	       (unless (bobp)
		 (c-clear-syn-tab (1- (point))))
	       (setq st-pos (point))
	       (and (< (point) end)
		    (not (eq (char-before) ?\")))))
      (goto-char (min no-st-pos st-pos))
      nil))

  (defun c-multiline-string-check-final-quote ()
    (let (pos-ll pos-lt)
      (save-excursion
	(goto-char (point-max))
	(skip-chars-backward "^\"")
	(while
	    (and
	     (not (bobp))
	     (cond
	      ((progn
		 (setq pos-ll (c-literal-limits)
		       pos-lt (c-literal-type pos-ll))
		 (memq pos-lt '(c c++)))
	       ;; In a comment.
	       (goto-char (car pos-ll)))
	      ((save-excursion
		 (backward-char)		; over "
		 (c-is-escaped (point)))
	       ;; At an escaped string.
	       (backward-char)
	       t)
	      (t
	       ;; At a significant "
	       (c-clear-syn-tab (1- (point)))
	       (setq pos-ll (c-literal-limits)
		     pos-lt (c-literal-type pos-ll))
	       nil)))
	  (skip-chars-backward "^\""))
	(cond
	 ((bobp))
	 ((eq pos-lt 'string)
	  (c-put-syn-tab (1- (point)) '(15)))
	 (t nil))))))

;;; End of fix for strings on version 27.1



(defvar csharp-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table csharp))
  "Syntax table used in csharp-mode buffers.")

(defvar csharp-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in csharp-mode buffers.")

(easy-menu-define csharp-mode-menu csharp-mode-map "C# Mode Commands"
  (cons "C#" (c-lang-const c-mode-menu csharp)))


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

(defconst csharp-compilation-re-dotnet-error
  "\\([^\r\n]+\\) : error [A-Z]+[0-9]+:")

(defconst csharp-compilation-re-dotnet-warning
  "\\([^\r\n]+\\) : warning [A-Z]+[0-9]+:")

(defconst csharp-compilation-re-dotnet-testfail
  (concat
   "^[[:blank:]]+X \\(?:.+\n\\)"
   "[[:blank:]]+Error Message:\n"
   "[[:blank:]]+\\(?:.+\n\\)"
   "\\(?:^Expected: \\(?:.+\n\\)\\)?"
   "\\(?:^Actual: \\(?:.+\n\\)\\)?"
   "[[:blank:]]+Stack Trace:\n"
   "[[:blank:]]+at [^\n]+ in \\([^\n]+\\):line \\([0-9]+\\)"))


(eval-after-load 'compile
  (lambda ()
    (dolist
        (regexp
         `((dotnet-testfail
            ,csharp-compilation-re-dotnet-testfail
            1 2)
           (xbuild-error
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
            (4 compilation-warning-face))
           (dotnet-error
            ,csharp-compilation-re-dotnet-error
            1)
           (dotnet-warning
            ,csharp-compilation-re-dotnet-warning
            1 nil nil 1)))
      (add-to-list 'compilation-error-regexp-alist-alist regexp)
      (add-to-list 'compilation-error-regexp-alist (car regexp)))))

;;; Tree-sitter

(defcustom csharp-mode-enable-tree-sitter nil
  "Use tree sitter for font locking and indentation."
  :type 'boolean)

(defvar-local csharp-mode-tree-sitter-patterns
  [ ;; Various constructs
   (comment) @comment
   (modifier) @keyword
   (this_expression) @keyword

   ;; Literals
   [(real_literal) (integer_literal)] @number
   (null_literal) @constant
   (boolean_literal) @constant
   (character_literal) @string

   ;; Keywords
   ["using" "namespace" "class" "if" "else" "throw" "new" "for"
    "return" "await" "struct" "enum" "switch" "case"
    "default" "typeof" "try" "catch" "finally" "break"
    "foreach" "in" "yield" "get" "set" "when" "as" "out"
    "is" "while" "continue" "this" "ref" "goto" "interface"
    "from" "where" "select"
    ] @keyword

   ;; Linq
   (from_clause (identifier) @variable)
   (group_clause)
   (order_by_clause)
   (select_clause (identifier) @variable)
   (query_continuation (identifier) @variable) @keyword

   ;; String
   (interpolation (identifier) (interpolation_format_clause) @variable)
   (interpolation (identifier)* @variable)
   [(string_literal) (verbatim_string_literal) (interpolated_string_expression)] @string

   ;; Enum
   (enum_member_declaration (identifier) @variable)
   (enum_declaration (identifier) @type)

   ;; Interface
   (interface_declaration
    name: (identifier) @type)

   ;; Struct
   (struct_declaration (identifier) @type)

   ;; Namespace
   (namespace_declaration
    name: (identifier) @type)

   ;; Class
   (base_list (identifier) @type)
   (property_declaration
    type: (nullable_type) @type
    name: (identifier) @variable)
   (property_declaration
    type: (predefined_type) @type
    name: (identifier) @variable)
   (property_declaration
    type: (identifier) @type
    name: (identifier) @variable)
   (class_declaration
    name: (identifier) @type)
   (constructor_declaration (identifier) @type)

   ;; Method
   (method_declaration (identifier) @type (identifier) @function)
   (method_declaration (predefined_type) @type (identifier) @function)
   (method_declaration (nullable_type) @type (identifier) @function)
   (method_declaration (void_keyword) @type (identifier) @function)
   (method_declaration (generic_name) (identifier) @function)

   ;; Function
   (local_function_statement (identifier) @type (identifier) @function)
   (local_function_statement (predefined_type) @type (identifier) @function)
   (local_function_statement (nullable_type) @type (identifier) @function)
   (local_function_statement (void_keyword) @type (identifier) @function)
   (local_function_statement (generic_name) (identifier) @function)
   
   ;; Parameter
   (parameter
    type: (identifier) @type
    name: (identifier) @variable)
   (parameter (identifier) @variable)

   ;; Array
   (array_rank_specifier (identifier) @variable) 
   (array_type (identifier) @type)
   (array_creation_expression)
   
   ;; Attribute
   (attribute (identifier) @variable (attribute_argument_list))
   (attribute (identifier) @variable)

   ;; Object init
   (anonymous_object_creation_expression)
   (object_creation_expression (identifier) @type)
   (initializer_expression (identifier) @variable)

   ;; Variable
   (variable_declaration (identifier) @type)
   (variable_declarator (identifier) @variable)

   ;; Equals value
   (equals_value_clause (identifier) @variable)

   ;; Return
   (return_statement (identifier) @variable)
   (yield_statement (identifier) @variable)
   
   ;; Type
   (type_parameter
    (identifier) @type)
   (type_argument_list
    (identifier) @type)
   (generic_name
    (identifier) @type)
   (implicit_type) @type
   (predefined_type) @type
   (nullable_type) @type
   ["operator"] @type

   ;; Exprs
   (binary_expression (identifier) @variable (identifier) @variable)
   (binary_expression (identifier)* @variable)
   (conditional_expression (identifier) @variable)
   (prefix_unary_expression (identifier)* @variable)
   (postfix_unary_expression (identifier)* @variable)
   (type_of_expression (identifier) @variable)
   (assignment_expression (identifier) @variable)
   (cast_expression (identifier) @type)
   
   ;; Preprocessor
   (preprocessor_directive) @constant
   (preprocessor_call (identifier) @string)

   ;; Loop
   (for_each_statement (identifier) @type (identifier) @variable)
   (for_each_statement (implicit_type) @type (identifier) @variable)
   (for_each_statement (predefined_type) @type (identifier) @variable)

   ;; Exception
   (catch_declaration (identifier) @type (identifier) @variable)
   (catch_declaration (identifier) @type)

   ;; Switch
   (switch_statement (identifier) @variable)
   (switch_expression (identifier) @variable)
   
   ;; If
   (if_statement (identifier) @variable)
   
   ;; Declaration expression
   (declaration_expression (implicit_type) (identifier) @variable)

   ;; Arrow expression
   (arrow_expression_clause (identifier) @variable)

   ;; Other
   (label_name) @variable
   (qualified_name (identifier) @type)
   (using_directive (identifier)* @type)
   (await_expression (identifier)* @function)
   (invocation_expression (identifier) @function)
   (element_access_expression (identifier) @variable)
   (conditional_access_expression (identifier) @variable)
   (member_binding_expression (identifier) @variable)
   (member_access_expression (identifier) @function)
   (name_colon (identifier)* @variable)
   (name_equals (identifier) @type)
   (field_declaration)
   (argument (identifier) @variable)
   ]
  "Default patterns for tree-sitter support.")

;;; Tree-sitter indentation

(defgroup csharp-mode-indent nil "Indent lines using Tree-sitter as backend"
  :group 'tree-sitter)

(defcustom csharp-mode-indent-offset 4
  "Indent offset for csharp-mode"
  :type 'integer
  :group 'csharp)

(defcustom csharp-mode-indent-scopes
  '((indent-all . ;; these nodes are always indented
                (accessor_declaration
                 break_statement
                 arrow_expression_clause
                 parameter_list
                 conditional_expression
                 "."))
    (indent-rest . ;; if parent node is one of these and node is not first → indent
                 (
                  binary_expression
                  switch_section
                  ))
    (indent-body . ;; if parent node is one of these and current node is in middle → indent
                 (block
                  anonymous_object_creation_expression
                  enum_member_declaration_list
                  initializer_expression
                  expression_statement
                  declaration_list
                  attribute_argument_list
                  switch_body))

    (paren-indent . ;; if parent node is one of these → indent to paren opener
                  (parenthesized_expression))
    (align-char-to . ;; chaining char → node types we move parentwise to find the first chaining char
                   ())
    (aligned-siblings . ;; siblings (nodes with same parent) should be aligned to the first child
                      (parameter))

    (multi-line-text . ;; if node is one of these, then don't modify the indent
                     ;; this is basically a peaceful way out by saying "this looks like something
                     ;; that cannot be indented using AST, so best I leave it as-is"
                     (comment
                      preprocessor_call
                      labeled_statement))
    (outdent . ;; these nodes always outdent (1 shift in opposite direction)
             (;; "}"
              case_switch_label
              ))
    )
  "Scopes for indenting in C#."
  :type 'sexp)

;;;; Private functions
(defun csharp-mode-indent--node-is-indent-all (node)
  "Non-nil if NODE type is in indent-all group.

Nodes in this group will be always +1 indentend."
  (let-alist csharp-mode-indent-scopes
    (member (tsc-node-type node)
            .indent-all)))

(defun csharp-mode-indent--node-is-indent-rest (node)
  "Non-nil if NODE type is in indent-rest group.

Nodes in this group will +1 indentend if they are a non-first
child of parent node."
  (let-alist csharp-mode-indent-scopes
    (member (tsc-node-type node)
            .indent-rest)))

(defun csharp-mode-indent--node-is-indent-body (node)
  "Non-nil if NODE type is in indent-body group.

Nodes in this group will +1 indentend if they are both a
non-first child of and non-last child of parent node."
  (let-alist csharp-mode-indent-scopes
    (member (tsc-node-type node)
            .indent-body)))

(defun csharp-mode-indent--node-is-multi-line-text (node)
  "Non-nil if NODE type is in indent-rest group.

Nodes in this group will keep their current indentation"
  (let-alist csharp-mode-indent-scopes
    (member (tsc-node-type node)
            .multi-line-text)))

(defun csharp-mode-indent--node-is-aligned-sibling (node)
  "Non-nil if NODE type is in aligned-siblings group.

Nodes in this group will be aligned to the column of the first sibling."
  (let-alist csharp-mode-indent-scopes
    (member (tsc-node-type node)
            .aligned-siblings)))

(defun csharp-mode-indent--highest-node-at-position (position)
  "Get the node at buffer POSITION that's at the highest level.

POSITION is a byte position in buffer like \\(point-min\\)."
  (save-excursion
    (goto-char position)
    ;; maybe implement this as a cl-loop
    (let* ((current-node (tree-sitter-node-at-point)))
      ;; move upwards until we either don't have aparent node
      ;; or we moved out of line
      (while (and
	      current-node
	      (when-let* ((parent-node (tsc-get-parent current-node)))
                (when (and ;; parent and current share same position
                       (eq (tsc-node-start-byte parent-node)
                           (tsc-node-start-byte current-node)))
		  ;; move upwards to the parent node
		  (setq current-node parent-node)))))
      current-node)))

(defun csharp-mode-indent--parentwise-path (node)
  "Get list of nodes by moving parent-wise starting at NODE.

The last element in returned path is NODE."
  (let ((next-parent-node (tsc-get-parent node))
        (path
         (list node)))
    (while next-parent-node
      ;; collect
      (push next-parent-node path)
      ;; move to next iteration
      (setq next-parent-node (tsc-get-parent next-parent-node)))
    path))

(defun csharp-mode-indent--node-is-paren-indent (node)
  "Non-nil if NODE type is in paren-indent group.

Child nodes in this group will be indentend to the paren opener
column."
  (let-alist csharp-mode-indent-scopes
    (member (tsc-node-type node)
            .paren-indent)))

(defun csharp-mode-indent--chain-column (current-node align-char-to-alist parentwise-path)
  "When node is in a chain call, return column to align each call.

CURRENT-NODE current node being indented
ALIGN-CHAR-TO-ALIST char → group of node types we can move within when searching
for the first chain char.
This group is supposed to contain all node types conformed by a chain.
PARENTWISE-PATH nodes from CURRENT-NODE to tree root (\"document\")

Returns a column to indent to or nil if no such column can / should be applied.

Reads text from current buffer."
  (let ((first-character-for-current-node
         (string-to-char
          (tsc-node-text current-node))))
    (when-let* ((last-parent-belongs-to
                 (alist-get first-character-for-current-node
                            align-char-to-alist))
                (last-parent-belonging-to
                 (thread-last parentwise-path
                   (reverse) ;; path starts at (ts-parent current-node)
                   (cdr) ;; skip current-node
                   ;; walk within allowed boundaries
                   (seq-take-while
                    (lambda (node)
                      (member (tsc-node-type node)
                              last-parent-belongs-to)))
                   (seq-first)))
                (first-char-position-within-last-parent-node
                 ;; naive search, could be updated later
                 ;; this may detect wrong column-char with something like ⎡a(should().ignore().this)\n.b()\n.c()⎦
                 (save-excursion
                   (goto-char
                    (tsc-node-start-byte last-parent-belonging-to))
                   (search-forward-regexp
                    (regexp-quote
                     (char-to-string
                      first-character-for-current-node))
                    (tsc-node-end-byte current-node)
                    t)
                   (- (point) 1)))
                (end-of-parent-line-pos
                 (save-excursion
                   (goto-char
                    (tsc-node-start-byte last-parent-belonging-to))
                   (line-end-position))))
      (when (and (numberp first-char-position-within-last-parent-node)
                 ;; char is within parent line
                 (or (< first-char-position-within-last-parent-node
                        end-of-parent-line-pos)
                     ;; char is the first in its line
                     (eq first-char-position-within-last-parent-node
                         (save-excursion
                           (goto-char
                            first-char-position-within-last-parent-node)
                           (back-to-indentation)
                           (point)))))
        ;; indent to column, which is (char-pos - line-begin-pos)
        (save-excursion
          (goto-char first-char-position-within-last-parent-node)
          (- first-char-position-within-last-parent-node
             (line-beginning-position)))))))

(defun csharp-mode-indent--first-sibling-column (current-node parent-node)
  "Column position for CURRENT-NODE's first sibling.

If CURRENT-NODE belongs to the aligned-siblings group, will look up the first
sibling in same group \\(running through PARENT-NODE's children) and return
its column.

CSHARP-MODE-INDENT-SCOPES is used to test whether
CURRENT-NODE belongs to the aligned-siblings group."
  (when (and parent-node
             (csharp-mode-indent--node-is-aligned-sibling current-node))
    (when-let* ((current-node-type
                 (tsc-node-type current-node))
                (first-sibling
                 (cl-loop for ith-sibling = (tsc-get-nth-child parent-node 0)
                          then (tsc-get-next-sibling ith-sibling)
                          while (not (null ith-sibling))
                          if (equal current-node-type
                                    (tsc-node-type ith-sibling))
                          return ith-sibling
                          end))
                (first-sibling-position
                 (tsc-node-start-byte first-sibling)))
      (when (not (tsc-node-eq current-node first-sibling))
        (save-excursion
          (goto-char first-sibling-position)
          (- first-sibling-position
             (line-beginning-position)))))))

(cl-defun csharp-mode-indent--indents-in-path (parentwise-path original-column)
  "Map PARENTWISE-PATH into indent instructions.

Each element of the returned list is one of the following

no-indent                         nothing to add to current column
indent                            add one indent to current column
outdent                           subtract one indent to current column
\(column-indent . COLUMN)         match parent's parent opener column
\(preserve . ORIGINAL-COLUMN)     preserve the column that was before

What is checked to add an indent:
- A node bolongs into the \"indent\" group in CSHARP-MODE-INDENT-SCOPES
- Deterimen what group the node's parent belongs to, and whether the node
is in a middle position.
- A node belongs to the \"outdent\" group in CSHARP-MODE-INDENT-SCOPES
- A node belongs to the \"column-indent\" group in CSHARP-MODE-INDENT-SCOPES"
  (let ((last-node
         (seq-elt
          parentwise-path
          (-
           (length parentwise-path)
           1))))
    (thread-last parentwise-path
      (seq-map
       (lambda (current-node)
         (let* ((previous-node
                 (tsc-get-prev-sibling current-node))
                (next-node
                 (tsc-get-next-sibling current-node))
                (parent-node
                 (tsc-get-parent current-node))
                (current-node-is-rest
                 previous-node)
                (current-node-is-middle-node
                 (and current-node-is-rest next-node))
                (current-node-must-indent
                 (csharp-mode-indent--node-is-indent-all current-node))
                (current-node-must-outdent
                 (and
                  (eq last-node current-node)
                  (csharp-mode-indent--node-is-outdent current-node)))
                (chain-column
                 (csharp-mode-indent--chain-column
                  current-node
                  (let-alist csharp-mode-indent-scopes
                    .align-char-to)
                  parentwise-path))
                (sibling-column
                 (csharp-mode-indent--first-sibling-column
                  current-node
                  parent-node)))
           (cond
            ((numberp chain-column)
             `(column-indent ,chain-column))
            ((numberp sibling-column)
             `(column-indent ,sibling-column))
            ((csharp-mode-indent--node-is-multi-line-text current-node)
             `(preserve . ,original-column))
            ((and parent-node
                  (csharp-mode-indent--node-is-paren-indent parent-node))
             (let* ((paren-opener
                     (tsc-node-start-byte parent-node))
                    (paren-point
                     (save-excursion
                       (goto-char paren-opener)
                       (point)))
                    (beginning-of-line-point
                     (save-excursion
                       (goto-char paren-opener)
                       (beginning-of-line 1)
                       (point)))
                    (paren-indenting-column
                     (+ 1
                        (- paren-point beginning-of-line-point))))
               `(column-indent ,paren-indenting-column)))

            ((or current-node-must-indent
                 (and parent-node
                      current-node-is-rest
                      (csharp-mode-indent--node-is-indent-rest parent-node))
                 (and parent-node
                      current-node-is-middle-node
                      (csharp-mode-indent--node-is-indent-body parent-node)))
             (if current-node-must-outdent
                 'no-indent ;; if it's an outdent, cancel
               'indent))
            (current-node-must-outdent
             'outdent)
            (t
             'no-indent))))))))

(defun csharp-mode-indent--node-is-outdent (node)
  "Return non-nil if NODE outdents per SCOPES.

NODE is tested if it belongs into the \"outdent\" group in SCOPES."
  (let-alist csharp-mode-indent-scopes
    (member (tsc-node-type node)
            .outdent)))

(defun csharp-mode-indent--updated-column (column indent)
  "Return COLUMN after added indent instructions per INDENT.

INDENT is one of `csharp-mode-indent--indents-in-path'.

If \"1 indent\" is to be applied, then returned value is
CSHARP-MODE-INDENT-OFFSET + INDENT."
  (pcase indent
    (`no-indent
     column)
    (`indent
     (+ column csharp-mode-indent-offset))
    (`outdent
     (- column csharp-mode-indent-offset))
    (`(column-indent ,paren-column)
     paren-column)
    (`(preserve . ,original-column)
     original-column)
    (_
     (error "Unexpected indent instruction: %s" indent))))

(cl-defun csharp-mode-indent--indent-column (original-column)
  "Return the column the first non-whitespace char at POSITION should indent to.

Collect indent instruction per AST with
`csharp-mode-indent--indents-in-path', then apply instructions
with `csharp-mode-indent--updated-column' using
CSHARP-MODE-INDENT-OFFSET as step.

See `csharp-mode-indent-line'.  ORIGINAL-COLUMN is forwarded to
`csharp-mode-indent--indents-in-path'"
  (let* ((indenting-node
          (csharp-mode-indent--highest-node-at-position
           (save-excursion
             (back-to-indentation)
             (point))))
         (parentwise-path (csharp-mode-indent--parentwise-path indenting-node))
         (indents-in-path
          (csharp-mode-indent--indents-in-path parentwise-path
                                               original-column)))
    (seq-reduce #'csharp-mode-indent--updated-column
                indents-in-path
                0 ;; start at column 0
                )))

;;;; Public API

;;;###autoload
(defun csharp-mode-indent-line ()
  "Use Tree-sitter as backend to indent current line."
  ;;Use in buffer like so:

  ;; (setq-local indent-line-function #'csharp-mode-indent-line).
  (let* ((original-position
          (point))
         (first-non-blank-pos ;; see savep in `smie-indent-line'
          (save-excursion
            (forward-line 0)
            (skip-chars-forward " \t")
            (point)))
         (should-save-excursion
          (< first-non-blank-pos original-position))
         (original-column
          (abs (- (line-beginning-position)
                  first-non-blank-pos)))
         (new-column
          (csharp-mode-indent--indent-column original-column)))
    (when (numberp new-column)
      (if should-save-excursion
          (save-excursion (indent-line-to new-column))
        (indent-line-to new-column)))))

(defun csharp-mode-indent-line-and-debug ()
  "Call `csharp-mode-indent-line' while printing useful info."
  (let* ((line-str (thing-at-point 'line))
         (position (point))
         (indenting-node (csharp-mode-indent--highest-node-at-position
                          position))
         (parentwise-path (csharp-mode-indent--parentwise-path indenting-node))
         (readable-parentwise-path
          (seq-map 'tsc-node-type parentwise-path))
         (tree-sitter-tree-before (tsc-tree-to-sexp tree-sitter-tree))
         (column
          (csharp-mode-indent-line)))
    (message "csharp-mode-indent: Indented ⎡%s⎦ to ⎡%s⎦ (col %s) because of parentwise path of ⎡%s⎦ (while looking at ⎡%s⎦ & when tree is ⎡%s⎦)"
             line-str
             (thing-at-point 'line)
             column
             readable-parentwise-path
             (tsc-node-type indenting-node)
             tree-sitter-tree-before)))

;;; End of tree-sitter



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

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
  :group 'csharp

  (if csharp-mode-enable-tree-sitter
      (progn
        (setq-local indent-line-function #'csharp-mode-indent-line)

        ;; https://github.com/ubolonton/emacs-tree-sitter/issues/84
        (unless font-lock-defaults
          (setq font-lock-defaults '(nil)))
        (setq-local tree-sitter-hl-default-patterns csharp-mode-tree-sitter-patterns)
        ;; Comments
        (setq-local comment-start "// ")
        (setq-local comment-end "")

        (tree-sitter-hl-mode))
    (progn
      :after-hook (c-update-modeline)
      (c-initialize-cc-mode t)
      (c-init-language-vars csharp-mode)
      (c-common-init 'csharp-mode)
      (easy-menu-add csharp-mode-menu)
      (setq-local c-doc-comment-style '((csharp-mode . codedoc)))
      (c-run-mode-hooks 'c-mode-common-hook 'csharp-mode-hook))))

(provide 'csharp-mode)

;;; csharp-mode.el ends here
