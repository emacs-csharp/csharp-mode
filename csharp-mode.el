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

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-indent)

(require 'compile)

(defgroup csharp nil
  "Major mode for editing C# code."
  :group 'prog-mode)

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

(defcustom csharp-indent-offset 4
  "Indent offset for csharp-mode"
  :type 'number
  :group 'csharp)

(defcustom tree-sitter-indent-csharp-scopes nil
  "Scopes for indenting in C#."
  :type 'sexp)

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
  :group 'csharp
  (setq tree-sitter-indent-csharp-scopes
        '((indent-all . ;; these nodes are always indented
                      (anonymous_object_creation_expression
                       switch_body
                       switch_section))
          (indent-rest . ;; if parent node is one of these and node is not first → indent
                       (namespace_declaration
                        using_statement
                        class_declaration
                        struct_declaration
                        method_declaration
                        object_creation_expression
                        array_creation_expression
                        lambda_expression
                        local_function_statement
                        enum_declaration
                        for_each_statement
                        if_statement
                        try_statement))
          (indent-body . ;; if parent node is one of these and current node is in middle → indent
                       ())

          (paren-indent . ;; if parent node is one of these → indent to paren opener
                        ())
          (align-char-to . ;; chaining char → node types we move parentwise to find the first chaining char
                         ())
          (aligned-siblings . ;; siblings (nodes with same parent) should be aligned to the first child
                            (parameter))

          (multi-line-text . ;; if node is one of these, then don't modify the indent
                           ;; this is basically a peaceful way out by saying "this looks like something
                           ;; that cannot be indented using AST, so best I leave it as-is"
                           (comment preprocessor_call label_name))
          (outdent . ;; these nodes always outdent (1 shift in opposite direction)
                   ("}"))
          )
        )
  (when (boundp 'electric-indent-inhibit)
    (setq electric-indent-inhibit t))
  (setq-local indent-line-function #'tree-sitter-indent-line-and-debug)

  ;; https://github.com/ubolonton/emacs-tree-sitter/issues/84
  (unless font-lock-defaults
    (setq font-lock-defaults '(nil)))
  (setq-local tree-sitter-hl-default-patterns
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
                "is" "while" "continue" "this" "ref" "goto"
                ] @keyword

                  ;; Linq
               (from_clause (identifier) @variable) @keyword
               (group_clause)
               (order_by_clause)
               (select_clause)
               (query_continuation (identifier) @variable) @keyword

               ;; String
               (interpolation (identifier) (interpolation_format_clause) @variable)
               (interpolation (identifier)* @variable)
               [(string_literal) (verbatim_string_literal) (interpolated_string_expression)] @string

               ;; Enum
               (enum_member_declaration (identifier) @variable)
               (enum_declaration (identifier) @type)

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
               ])
  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (tree-sitter-hl-mode))

(provide 'csharp-mode)

;;; csharp-mode.el ends here
