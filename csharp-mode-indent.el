;;; csharp-mode-indent.el --- Provide indentation with a Tree-sitter backend -*- lexical-binding: t; -*-

;; Copyright © 2020  Felipe Lema

;; Author: Felipe Lema <felipelema@mortemale.org>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Keywords: convenience, internal
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.12.1") (seq "2.21"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'tree-sitter)

(defgroup csharp-mode-indent nil "Indent lines using Tree-sitter as backend"
  :group 'tree-sitter)

(defcustom csharp-mode-indent-offset 4
  "Indent offset for csharp-mode"
  :group 'csharp)

(defcustom csharp-mode-indent-scopes
  '((indent-all . ;; these nodes are always indented
                (accessor_declaration
                 break_statement
                 "."))
    (indent-rest . ;; if parent node is one of these and node is not first → indent
                 (
                  switch_section
                  ))
    (indent-body . ;; if parent node is one of these and current node is in middle → indent
                 (block
                  anonymous_object_creation_expression
                  enum_member_declaration_list
                  initializer_expression
                  expression_statement
                  declaration_list
                  switch_body))

    (paren-indent . ;; if parent node is one of these → indent to paren opener
                  ())
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

(provide 'csharp-mode-indent)
;;; csharp-mode-indent.el ends here
