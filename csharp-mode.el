;;; csharp-mode.el --- C# mode derived mode

;; Author     : Dylan R. E. Moonfire (original)
;; Maintainer : Jostein Kjønigsen <jostein@gmail.com>
;; Created    : February 2005
;; Modified   : 2018
;; Version    : 0.9.2
;; Keywords   : c# languages oop mode
;; X-URL      : https://github.com/josteink/csharp-mode
;; Last-saved : 2018-Jul-08

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;    This is a major mode for editing C# code.  It performs automatic
;;    indentation of C# syntax; font locking; and integration with
;;    imenu.el.
;;
;;    csharp-mode requires CC Mode 5.30 or later.  It works with
;;    cc-mode 5.31.3, which is current at this time.
;;
;; Features:
;;
;;   - font-lock and indent of C# syntax including:
;;       all c# keywords and major syntax
;;       attributes that decorate methods, classes, fields, properties
;;       enum types
;;       #if/#endif  #region/#endregion
;;       instance initializers
;;       anonymous functions and methods
;;       verbatim literal strings (those that begin with @)
;;       generics
;;
;;   - automagic code-doc generation when you type three slashes.
;;
;;   - compatible with electric-pair-mode for intelligent insertion
;;     of matched braces, quotes, etc.
;;
;;   - imenu integration - generates an index of namespaces, classes,
;;     interfaces, methods, and properties for easy navigation within
;;     the buffer.
;;

;;  General
;;  ----------------------------
;;
;;  Mostly C# mode will "just work."  Use `describe-mode' to see the
;;  default keybindings and the highlights of the mode.
;;
;;
;;  imenu integration
;;  -----------------------------
;;
;;  This should just work.  For those who don't know what imenu is, it
;;  allows navigation to different points within the file from an
;;  "Index" menu, in the window's menubar.  csharp-mode computes the
;;  menu containing the namespaces, classes, methods, and so on, in the
;;  buffer.  This happens at the time the file is loaded; for large
;;  files it takes a bit of time to complete the scan.  If you don't
;;  want this capability, set `csharp-want-imenu' to nil.
;;
;;


;;; Known Bugs:
;;
;;   The imenu scan is text-based and naive. For example, if you
;;   intersperse comments between the name of a class/method/namespace,
;;   and the curly brace, the scan will not recognize the thing being
;;   declared. This is fixable - would need to extract the buffer
;;   substring then remove comments before doing the regexp checks - but
;;   it would make the scan much slower.  Also, the scan doesn't deal
;;   with preproc symbol definitions and #if/#else. Those things are
;;   invisible to the scanner csharp-mode uses to build the imenu menu.
;;
;;   Leading identifiers are no longer being fontified, for some reason.
;;   See matchers-before. (Not sure this is still a problem - 19 may
;;   2011 DPC)
;;
;;   Method names with a preceding attribute are not fontified.
;;
;;   The symbol following #if is not fontified.  It should be treated like
;;   define and get font-lock-variable-name-face .
;;
;;   This code doesn't seem to work when you compile it, then
;;   load/require in the emacs file. You will get an error (error
;;   "`c-lang-defconst' must be used in a file") which happens because
;;   cc-mode doesn't think it is in a buffer while loading directly
;;   from the init. However, if you call it based on a file extension,
;;   it works properly. Interestingly enough, this doesn't happen if
;;   you don't byte-compile cc-mode.
;;
;;
;;
;;  Todo:
;;
;;   imenu should scan for and find delegates and events, in addition
;;   to the classes, structs, properties and methods it does currently.
;;
;;   Get csharp-mode.el accepted as part of the emacs standard distribution.
;;   Must contact monnier at iro.umontreal.ca to make this happen.
;;
;;  Acknowledgements:
;;
;;    Thanks to Alan Mackenzie and Stefan Monnier for answering questions
;;    and making suggestions. And to Trey Jackson for sharing his
;;    knowledge of emacs lisp.
;;
;;

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;    0.2.0 - Fixed the identification on the "enum" keyword.
;;          - Fixed the font-lock on the "base" keyword
;;    0.3.0 - Added a regex to fontify attributes. It isn't the
;;            the best method, but it handles single-like attributes
;;            well.
;;          - Got "super" not to fontify as a keyword.
;;          - Got extending classes and interfaces to fontify as something.
;;    0.4.0 - Removed the attribute matching because it broke more than
;;            it fixed.
;;          - Corrected a bug with namespace not being properly identified
;;            and treating the class level as an inner object, which screwed
;;            up formatting.
;;          - Added "partial" to the keywords.
;;    0.5.0 - Found bugs with compiled cc-mode and loading from init files.
;;          - Updated the eval-when-compile to code to let the mode be
;;            compiled.
;;    0.6.0 - Added the c-filter-ops patch for 5.31.1 which made that
;;            function in cc-langs.el unavailable.
;;          - Added a csharp-lineup-region for indentation #region and
;;            #endregion block differently.
;;    0.7.0 - Added autoload so update-directory-autoloads works
;;            (Thank you, Nikolaj Schumacher)
;;          - Fontified the entire #region and #endregion lines.
;;          - Initial work to get get, set, add, remove font-locked.
;;    0.7.1 - Added option to indent #if/endif with code
;;          - Fixed c-opt-cpp-prefix defn (it must not include the BOL
;;            char (^).
;;          - proper fontification and indent of classes that inherit
;;            (previously the colon was confusing the parser)
;;          - reclassified namespace as a block beginner
;;          - removed $ as a legal symbol char - not legal in C#.
;;          - added struct to c-class-decl-kwds so indent is correct
;;            within a struct.
;;    0.7.2 - Added automatic codedoc insertion.
;;    0.7.3 - Instance initializers (new Type { ... } ) and
;;            (new Type() { ...} ) are now indented properly.
;;          - proper fontification and indent of enums as brace-list-*,
;;            including special treatment for enums that explicitly
;;            inherit from an int type. Previously the colon was
;;            confusing the parser.
;;          - proper fontification of verbatim literal strings,
;;            including those that end in slash. This edge case was not
;;            handled at all before; it is now handled correctly.
;;          - code cleanup and organization; removed the formfeed.
;;          - intelligent curly-brace insertion with
;;            `csharp-insert-open-brace'
;;    0.7.4 - added a C# style
;;          - using is now a keyword and gets fontified correctly
;;          - fixed a bug that had crept into the codedoc insertion.
;;    0.7.5 - now fontify namespaces in the using statements. This is
;;            done in the csharp value for c-basic-matchers-before .
;;          - also fontify the name following namespace decl.
;;            This is done in the csharp value for c-basic-matchers-after .
;;          - turn on recognition of generic types. They are now
;;            fontified correctly.
;;          - <> are now treated as syntactic parens and can be jumped
;;            over with c-forward-sexp.
;;          - Constructors are now fontified.
;;          - Field/Prop names inside object initializers are now fontified.
;;
;;    0.7.7 - relocate running c-run-mode-hooks to the end of
;;            csharp-mode, to allow user to modify key bindings in a
;;            hook if he doesn't like the defaults.
;;
;;    0.7.8 - redefine csharp-log to insert timestamp.
;;          - Fix byte-compile errors on emacs 23.2 ?  Why was
;;            c-filter-ops duplicated here?  What was the purpose of its
;;            presence here, I am not clear.
;;
;;    0.8.0 - include flymake magic into this module.
;;          - include yasnippet integration
;;
;;    0.8.2 2011 April DPC
;;          - small tweaks; now set a one-time bool for flymake installation
;;          - some doc updates on flymake
;;
;;    0.8.3 2011 May 17  DPC
;;          - better help on csharp-mode
;;          - csharp-move-* functions for manual navigation.
;;          - imenu integration for menu-driven navigation - navigate to
;;            named methods, classes, etc.
;;          - adjusted the flymake regexp to handle output from fxcopcmd,
;;            and extended the help to provide examples how to use this.
;;
;;    0.8.4 DPC 2011 May 18
;;          - fix a basic bug in the `csharp-yasnippet-fixup' fn.
;;
;;    0.8.5 DPC 2011 May 21
;;          - imenu: correctly parse Properties that are part of an
;;            explicitly specified interface. Probably need to do this
;;            for methods, too.
;;          - fontify the optional alias before namespace in a using (import).
;;          - Tweak open-curly magic insertion for object initializers.
;;          - better fontification of variables and references
;;          - "sealed" is now fontified as a keyword
;;          - imenu: correctly index ctors that call this or base.
;;          - imenu: correctly index Extension methods (this System.Enum e)
;;          - imenu: correctly scan  method params tagged with out, ref, params
;;          - imenu scan: now handle curlies within strings.
;;          - imenu: split menus now have better labels, are sorted correctly.
;;
;;    0.8.6 DPC 2011 May ??
;;          - extern keyword
;;
;;    0.8.7 2014 November 29
;;          - Fix broken cl-dependency in emacs24.4 and defadvice for tooltips.
;;
;;    0.8.8 2014 December 3
;;          - Fix broken byte-compile.
;;          - Add extra C# keywords.
;;          - Call prog-mode hooks.
;;
;;    0.8.9 2015 March 15
;;          - (Re)add compilation-mode support for msbuild and xbuild.
;;
;;    0.8.10 2015 May 31th
;;          - Imenu: Correctly handle support for default-values in paramlist.
;;
;;    0.8.11 2015 November 21st
;;          - Make mode a derived mode. Improve evil-support.
;;          - Add support for devenv compilation-output.
;;          - Fix all runtime warnings
;;          - Fix error with string-values in #region directives.
;;
;;    0.8.12 2016 January 6th
;;          - Various fixes and improvements for imenu indexing.
;;
;;    0.9.0 2016 September 9th
;;          - Fix issues with compilation-mode and lines with arrays.
;;          - Fontification of compiler directives.
;;          - Much faster, completely rewritten imenu-implementation.
;;          - Fix indentation issues.
;;          - Fix Emacs-25 related bugs.
;;          - Cleaned up dead code.
;;
;;    0.9.1 2017
;;          - Fix indentation for generic type-initializers.
;;          - Fix fontification of using and namespace-statements with
;;            underscores in them.
;;          - Fixes for indentation for many kinds of type-initializers.
;;
;;    0.9.2 2018 July
;;          - Try to fix some breakage introduced by changes in Emacs 27.
;;
;;; Code:

(require 'cc-mode)

(require 'imenu)

(defgroup csharp nil
  "Major mode for editing C# code."
  :group 'prog-mode)

;; Custom variables

(defcustom csharp-mode-hook nil
  "*Hook called by `csharp-mode'."
  :type 'hook
  :group 'csharp)

(defcustom csharp-want-imenu t
  "*Whether to generate a buffer index via imenu for C# buffers."
  :type 'boolean :group 'csharp)

;; ==================================================================
;;; imenu stuff

(defconst csharp--imenu-expression
  (let* ((single-space                   "[ \t\n\r\f\v]")
         (optional-space                 (concat single-space "*"))
         (bol                            "^[ \t]*") ;; BOL shouldn't accept lineshift.
         (space                          (concat single-space "+"))
         (access-modifier (regexp-opt '( "public" "private" "protected" "internal"
                                         "static" "sealed" "partial" "override" "virtual"
                                         "abstract" "async" "new" "unsafe")))
         ;; this will allow syntactically invalid combinations of modifiers
         ;; but that's a compiler problem, not a imenu-problem
         (access-modifier-list           (concat "\\(?:" access-modifier space "\\)"))
         (access-modifiers (concat access-modifier-list "*"))
         (basic-type                     (concat
                                          ;; typename
                                          "\\(?:[A-Za-z_][[:alnum:]_]*\\.\\)*"
                                          "[A-Za-z_][[:alnum:]_]*"
                                          ))
         (type                           (concat
                                          basic-type
                                          ;; simplified, optional generic constraint.
                                          ;; handles generic sub-types.
                                          "\\(?:<[[:alnum:],<> \t\n\f\v\r]+>\\)?"))
         (return-type                    (concat
                                          type
                                          ;; optional array-specifier
                                          "\\(?:\\[\\]\\)?"))
         (interface-prefix               (concat "\\(?:" type "\\.\\)"))
         ;; param-list with parens
         (parameter-list "\\(?:\([^!\)]*\)\\)")
         (inheritance-clause (concat "\\(?:"
                                     optional-space
                                     ":"
                                     optional-space type
                                     "\\(?:" optional-space "," optional-space type "\\)*"
                                     "\\)?")))

    (list (list "namespace"
                (concat bol "namespace" space
                        "\\(" basic-type "\\)") 1)
          ;; not all these are classes, but they can hold other
          ;; members, so they are treated uniformly.
          (list "class"
                (concat bol
                        access-modifiers
                        "\\("
                        (regexp-opt '("class" "struct" "interface")) space
                        type inheritance-clause "\\)")  1)
          (list "enum"
                (concat bol
                        access-modifiers
                        "\\(" "enum" space
                        basic-type "\\)")  1)
          (list "ctor"
                (concat bol
                        ;; ctor MUST have access modifiers, or else we pick
                        ;; every if statement in the file...
                        access-modifier-list "+"
                        "\\("
                        basic-type
                        optional-space
                        parameter-list
                        "\\)"
                        "\\(?:"
                        optional-space
                        ":"
                        optional-space
                        "\\(?:this\\|base\\)"
                        optional-space
                        parameter-list
                        "\\)?"
                        optional-space "{") 1)
          (list "method"
                (concat bol
                        ;; we MUST require modifiers, or else we cannot reliably
                        ;; identify declarations, without also dragging in lots of
                        ;; if statements and what not.
                        access-modifier-list "+"
                        return-type space
                        "\\("
                        type
                        optional-space
                        parameter-list
                        "\\)"
                        ;; optional // or /* comment at end
                        "\\(?:[ \t]*/[/*].*\\)?"
                        optional-space
                        "{") 1)
          (list "method-inf"
                (concat bol
                        return-type space
                        "\\("
                        interface-prefix
                        type
                        optional-space
                        parameter-list
                        "\\)"
                        ;; optional // or /* comment at end
                        "\\(?:[ \t]*/[/*].*\\)?"
                        optional-space
                        "{") 1)
          (list "method-abs-ext"
                (concat bol
                        access-modifier-list "+"
                        (regexp-opt '("extern" "abstract")) space
                        return-type space
                        "\\("
                        type
                        optional-space
                        parameter-list
                        "\\)"
                        optional-space
                        ;; abstract/extern methods are terminated with ;
                        ";") 1)
          ;; delegates are almost like abstract methods, so pick them up here
          (list "delegate"
                (concat bol
                        access-modifiers
                        "delegate" space
                        return-type space
                        "\\("
                        type
                        "\\)"
                        optional-space
                        parameter-list
                        ;; optional // or /* comment at end
                        optional-space
                        ";") 1)
          (list "prop"
                (concat bol
                        ;; must require access modifiers, or else we
                        ;; pick up pretty much anything.
                        access-modifiers
                        return-type space
                        "\\("
                        type
                        "\\)"
                        optional-space "{" optional-space
                        ;; unless we are super-specific and expect the accessors,
                        ;; lots of weird things gets slurped into the name.
                        ;; including the accessors themselves.
                        (regexp-opt '("get" "set"))
                        ) 1)
          (list "prop-inf"
                (concat bol
                        return-type space
                        "\\("
                        interface-prefix
                        type
                        "\\)"
                        optional-space "{" optional-space
                        ;; unless we are super-specific and expect the accessors,
                        ;; lots of weird things gets slurped into the name.
                        ;; including the accessors themselves.
                        (regexp-opt '("get" "set"))
                        ) 1)
          ;; adding fields... too much?
          (list "field"
                (concat bol
                        access-modifier-list "+"
                        ;; fields can be readonly/const/volatile
                        "\\(?:" (regexp-opt '("readonly" "const" "volatile")) space "\\)?"
                        return-type space
                        "\\("
                        type
                        "\\)"
                        optional-space
                        ;; optional assignment
                        "\\(?:=[^;]+\\)?"
                        ";") 1)
          (list "indexer"
                (concat bol
                        access-modifiers
                        return-type space
                        "this" optional-space
                        "\\("
                        ;; opening bracket
                        "\\[" optional-space
                        ;; type
                        "\\([^\]]+\\)" optional-space
                        type
                        ;; closing brackets
                        "\\]"
                        "\\)"
                        optional-space "{" optional-space
                        ;; unless we are super-specific and expect the accessors,
                        ;; lots of weird things gets slurped into the name.
                        ;; including the accessors themselves.
                        (regexp-opt '("get" "set"))) 1)
          (list "event"
                (concat bol
                        access-modifier-list "+"
                        optional-space "event" optional-space
                        "\\("
                        return-type space
                        type
                        "\\)"
                        optional-space
                        ";") 1))))

(defun csharp--imenu-get-pos (pair)
  "Return `position' from a (title . position) cons-pair `PAIR'.

   The position may be a integer, or a marker (as returned by
   imenu-indexing).  This function ensures what is returned is an
   integer which can be used for easy comparison."
  (let ((pos (cdr pair)))
    (if (markerp pos)
        (marker-position pos)
      pos)))

(defun csharp--imenu-get-container (item containers previous)
  "Return the container which `ITEM' belongs to.

   `ITEM' is a (title . position) cons-pair.  `CONTAINERS' is a
   list of such.  `PREVIOUS' is the name of the previous
   container found when recursing through `CONTAINERS'.

   The final result is based on item's position relative to those
   found in `CONTAINERS', or nil if none is found."
  (if (not containers)
      previous
    (let* ((item-pos (csharp--imenu-get-pos item))
           (container (car containers))
           (container-pos (csharp--imenu-get-pos container))
           (rest      (cdr containers)))
      (if (and container-pos
               (< item-pos container-pos))
          previous
        (csharp--imenu-get-container item rest container)))))

(defun csharp--imenu-get-container-name (item containers)
  "Return the name of the container which `ITEM' belongs to.

   `ITEM' is a (title . position) cons-pair.
   `CONTAINERS' is a list of such.

   The name is based on the results from
   `csharp--imenu-get-container'."
  (let ((container (csharp--imenu-get-container item containers nil)))
    (if (not container)
        nil
      (let ((container-p1 (car (split-string (car container))))   ;; namespace
            (container-p2 (cadr (split-string (car container))))) ;; class/interface
        ;; use p1 (namespace) when there is no p2
        (if container-p2
            container-p2
          container-p1)))))

(defun csharp--imenu-sort (items)
  "Sort an imenu-index list `ITEMS' by the string-portion."
  (sort items (lambda (item1 item2)
                (string< (car item1) (car item2)))))

(defun csharp--imenu-get-class-name (class namespaces)
  "Gets a name for a imenu-index `CLASS'.

   Result is based on its own name and `NAMESPACES' found in the same file."
  (let ((namespace (csharp--imenu-get-container-name class namespaces))
        (class-name (car class)))
    (if (not namespace)
        class-name
      ;; reformat to include namespace
      (let* ((words (split-string class-name))
             (type  (car words))
             (name  (cadr words)))
        (concat type " " namespace "." name)))))

(defun csharp--imenu-get-class-nodes (classes namespaces)
  "Create a new alist with CLASSES as root nodes with NAMESPACES added.

   Each class will have one imenu index-entry \"( top)\" added by
   default."

  (mapcar (lambda (class)
            (let ((class-name (csharp--imenu-get-class-name class namespaces))
                  (class-pos  (cdr class)))
              ;; construct a new alist-entry where value is itself
              ;; a list of alist-entries with -1- entry which the top
              ;; of the class itself.
              (cons class-name
                    (list
                     (cons "( top )" class-pos)))))
          classes))

(defun csharp--imenu-get-class-node (result item classes namespaces)
  "Get the class-node in `RESULT' which an `ITEM' should be inserted into.

   For this calculation, the original index items `CLASSES' and `NAMESPACES'
   is needed."
  (let* ((class-item (csharp--imenu-get-container item classes nil))
         (class-name (csharp--imenu-get-class-name class-item namespaces)))
    (assoc class-name result)))

(defun csharp--imenu-format-item-node (item type)
  "Format an ITEM with a specified TYPE as an imenu item to be inserted into the index."
  (cons
   (concat "(" type ") " (car item))
   (cdr item)))

(defun csharp--imenu-append-items-to-menu (result key name index classes namespaces)
  "Formats the imenu-index using the provided values.

This is done by modifying the contents of `RESULT' in place."
  ;; items = all methods, all events, etc based on "type"
  (let* ((items (cdr (assoc key index))))
    (dolist (item items)
      (let ((class-node (csharp--imenu-get-class-node result item classes namespaces))
            (item-node  (csharp--imenu-format-item-node item name)))
        (nconc class-node (list item-node))))))

(defun csharp--imenu-transform-index (index)
  "Transform an imenu INDEX based on `IMENU-GENERIC-EXPRESSION'.

  The resulting structure should be based on full type-names, with
  type-members nested hierarchially below its parent.

  See `csharp-mode-tests.el' for examples of expected behaviour
  of such transformations."
  (let* ((result nil)
         (namespaces (cdr (assoc "namespace" index)))
         (classes    (cdr (assoc "class"     index)))
         (class-nodes (csharp--imenu-get-class-nodes classes namespaces)))
    ;; be explicit about collection variable
    (setq result class-nodes)
    (dolist (type '(("ctor")
                    ("method")
                    ("method-inf" "method")
                    ("method-abs-ext" "method")
                    ("prop")
                    ("prop-inf" "prop")
                    ("field")
                    ("event")
                    ("indexer")))
      (let* ((key (car type))
             (name (car (last type))))
        (csharp--imenu-append-items-to-menu result key name index classes namespaces)))

    ;; add enums and delegates to main result list, as own items.
    ;; We don't support nested types. EOS.
    ;;
    ;; This has the issue that they get reported as "function" in
    ;; `helm-imenu', but there's nothing we can do about that.
    ;; The alternative is making it a menu with -1- submenu which
    ;; says "( top )" but that will be very clicky...

    ;; before adding delegates, we need to pad the entry so that it
    ;; matches the "<type> <name>" signature used by all the other
    ;; imenu entries
    (let ((delegates (cdr (assoc "delegate" index))))
      (dolist (delegate delegates)
        (setf (car delegate) (concat "delegate " (car delegate)))))

    (dolist (type '("enum" "delegate"))
      (dolist (item (cdr (assoc type index)))
        (let ((item-name (csharp--imenu-get-class-name item namespaces)))
          (setq result (cons (cons item-name (cdr item))
                             result)))))

    ;; sort individual sub-lists
    (dolist (item result)
      (when (listp (cdr item))
        (setf (cdr item) (csharp--imenu-sort (cdr item)))))

    ;; sort main list
    ;; (Enums always sort last though, because they don't have
    ;; sub-menus)
    (csharp--imenu-sort result)))

(defun csharp--imenu-create-index-function ()
  "Create an imenu index."
  (csharp--imenu-transform-index
   (imenu--generic-function csharp--imenu-expression)))

(defun csharp--setup-imenu ()
  "Set up `imenu' for `csharp-mode'."

  ;; There are two ways to do imenu indexing. One is to provide a
  ;; function, via `imenu-create-index-function'.  The other is to
  ;; provide imenu with a list of regexps via
  ;; `imenu-generic-expression'; imenu will do a "generic scan" for you.
  ;;
  ;; We use both.
  ;;
  ;; First we use the `imenu-generic-expression' to build a index for
  ;; us, but we do so inside a `imenu-create-index-function'
  ;; implementation which allows us to tweak the results slightly
  ;; before returning it to Emacs.
  (setq imenu-create-index-function #'csharp--imenu-create-index-function)
  (imenu-add-menubar-index))


;;; Autoload mode trigger
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))


;;;###autoload
(define-derived-mode csharp-mode prog-mode "C#"
  "Major mode for editing C# code.

Key bindings:
\\{csharp-mode-map}"
  :after-hook (c-update-modeline)
  (c-initialize-cc-mode t)
  (c-common-init 'java-mode)

  (c-init-language-vars-for 'java-mode))

(provide 'csharp-mode)

;;; csharp-mode.el ends here
