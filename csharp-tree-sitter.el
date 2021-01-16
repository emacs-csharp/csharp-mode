;;; csharp-tree-sitter.el --- tree sitter support for C#  -*- lexical-binding: t; -*-

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Jostein Kjønigsen <jostein@gmail.com>
;;            : Theodor Thornhill <theo@thornhill.no>
;; Created    : September 2020
;; Modified   : 2020
;; Version    : 0.11.0
;; Keywords   : c# languages oop mode
;; X-URL      : https://github.com/emacs-csharp/csharp-mode
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.12.1") (tree-sitter-indent "0.1") (tree-sitter-langs "0.9.1"))

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
(require 'seq nil t)

(require 'tree-sitter nil 't)
(require 'tree-sitter-hl nil 't)
(require 'tree-sitter-indent nil 't)
(require 'tree-sitter-langs nil 't)

(require 'csharp-compilation)

(defvar csharp-mode-syntax-table)
(defvar csharp-mode-map)

(defvar tree-sitter-indent-current-scopes)
(defvar tree-sitter-indent-offset)
(defvar tree-sitter-hl-default-patterns)
(defvar tree-sitter-major-mode-language-alist)

(declare-function tree-sitter-hl-mode "ext:tree-sitter-hl.el")

;;; Tree-sitter

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
    "from" "where" "select" "lock" "base" "record" "init"
    "with"
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

   ;; Record
   (record_declaration (identifier) @type)

   (with_expression
    (with_initializer_expression
     (simple_assignment_expression
      (identifier) @variable)))

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
   (method_declaration (qualified_name (identifier) @type) (identifier) @function)

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

   ;; Lock statement
   (lock_statement (identifier) @variable)

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

(defvar csharp-mode-indent-scopes
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
                 (enum_member_declaration_list
                  base_list
                  block
                  anonymous_object_creation_expression
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
  "Scopes for indenting in C#.")

(defvar csharp-tree-sitter-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in csharp-mode buffers.")

(defvar csharp-tree-sitter-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?@ "_" table)
    table))

;;;###autoload
(define-derived-mode csharp-tree-sitter-mode prog-mode "C#"
  "Major mode for editing Csharp code.

Key bindings:
\\{csharp-tree-sitter-mode-map}"
  :group 'csharp
  :syntax-table csharp-tree-sitter-mode-syntax-table
  (if (not (featurep 'tree-sitter))
      (user-error "Please install package `tree-sitter` for such support")
    (setq-local tree-sitter-indent-current-scopes csharp-mode-indent-scopes)
    (setq-local tree-sitter-indent-offset csharp-mode-indent-offset)
    (setq-local indent-line-function #'tree-sitter-indent-line)

    ;; https://github.com/ubolonton/emacs-tree-sitter/issues/84
    (unless font-lock-defaults
      (setq font-lock-defaults '(nil)))
    (setq-local tree-sitter-hl-default-patterns csharp-mode-tree-sitter-patterns)
    ;; Comments
    (setq-local comment-start "// ")
    (setq-local comment-end "")

    (tree-sitter-hl-mode)))

(with-eval-after-load 'tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(csharp-tree-sitter-mode . c-sharp)))

(provide 'csharp-tree-sitter)

;;; csharp-tree-sitter.el ends here
