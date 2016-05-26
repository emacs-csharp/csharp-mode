(require 'csharp-mode)
(require 'imenu)

(defun csharp--on-defun-close-curly-p ()
   "return t when point is on the close-curly of a method."
   (and (looking-at "}")
        (save-excursion
          (and
           (progn (forward-char) (forward-sexp -1) t)
           (not (looking-back (csharp--regexp 'class-start) nil))
           (not (looking-back (csharp--regexp 'namespace-start) nil))
           (looking-back (csharp--regexp 'func-start) nil)))))

(defun csharp--on-ctor-close-curly-p ()
  "return t when point is on the close-curly of a constructor."
  (and (looking-at "}")
       (save-excursion
         (and
          (progn (forward-char) (forward-sexp -1) t)
          (looking-back (csharp--regexp 'ctor-start) nil)))))

(defun csharp--on-class-close-curly-p ()
  "return t when point is on the close-curly of a class or struct."
  (and (looking-at "}")
       (save-excursion
         (and
          (progn (forward-char) (forward-sexp -1) t)
          (not (looking-back (csharp--regexp 'namespace-start) nil))
          (looking-back (csharp--regexp 'class-start) nil)))))

(defun csharp--on-intf-close-curly-p ()
  "return t when point is on the close-curly of an interface."
  (and (looking-at "}")
       (save-excursion
         (and
          (progn (forward-char) (forward-sexp -1) t)
          (looking-back (csharp--regexp 'intf-start) nil)))))

(defun csharp--on-enum-close-curly-p ()
  "return t when point is on the close-curly of an enum."
  (and (looking-at "}")
       (save-excursion
         (and
          (progn (forward-char) (forward-sexp -1) t)
          (looking-back (csharp--regexp 'enum-start) nil)))))

(defun csharp--on-namespace-close-curly-p ()
  "return t when point is on the close-curly of a namespace."
  (and (looking-at "}")
       (save-excursion
         (and
          (progn (forward-char) (forward-sexp -1) t)
          (looking-back (csharp--regexp 'namespace-start) nil)))))

(defun csharp--on-defun-open-curly-p ()
  "return t when point is on the open-curly of a method."
  (and (looking-at "{")
       (not (looking-back (csharp--regexp 'class-start) nil))
       (not (looking-back (csharp--regexp 'namespace-start) nil))
       (looking-back (csharp--regexp 'func-start) nil)))

(defun csharp--on-genclass-open-curly-p ()
  "return t when point is on the open-curly of a generic class."
  (and (looking-at "{")
       (looking-back (csharp--regexp 'genclass-start) nil)))

(defun csharp--on-namespace-open-curly-p ()
  "return t when point is on the open-curly of a namespace."
  (and (looking-at "{")
       (looking-back (csharp--regexp 'namespace-start) nil)))

(defun csharp--on-ctor-open-curly-p ()
  "return t when point is on the open-curly of a ctor."
  (and (looking-at "{")
       (looking-back (csharp--regexp 'ctor-start) nil)))

(defun csharp--on-intf-open-curly-p ()
  "return t when point is on the open-curly of a interface."
  (and (looking-at "{")
       (looking-back (csharp--regexp 'intf-start) nil)))

(defun csharp--on-prop-open-curly-p ()
  "return t when point is on the open-curly of a property."
  (and (looking-at "{")
       (not (looking-back (csharp--regexp 'class-start) nil))
       (looking-back (csharp--regexp 'prop-start) nil)))

(defun csharp--on-indexer-open-curly-p ()
  "return t when point is on the open-curly of a C# indexer."
  (and (looking-at "{")
       (looking-back (csharp--regexp 'indexer-start) nil)))

(defun csharp--on-enum-open-curly-p ()
  "return t when point is on the open-curly of a interface."
  (and (looking-at "{")
       (looking-back (csharp--regexp 'enum-start) nil)))


;; define some advice for menu construction.

;; The way imenu constructs menus from the index alist, in
;; `imenu--split-menu', is ... ah ... perplexing.  If the csharp
;; create-index fn returns an ordered menu, and the imenu "sort" fn has
;; been set to nil, imenu still sorts the menu, according to the rule
;; that all submenus must appear at the top of any menu. Why?  I don't
;; know. This advice disables that weirdness in C# buffers.

(defadvice imenu--split-menu (around
                              csharp--imenu-split-menu-patch
                              activate compile)
  ;; This advice will run in all buffers.  Let's may sure we
  ;; actually execute the important bits only when a C# buffer is active.
  (if (and (string-match "\\.[Cc][Ss]$"  (file-relative-name buffer-file-name))
           (boundp 'csharp-want-imenu)
           csharp-want-imenu)
      (let ((menulist (copy-sequence menulist))
            keep-at-top)
        (if (memq imenu--rescan-item menulist)
            (setq keep-at-top (list imenu--rescan-item)
                  menulist (delq imenu--rescan-item menulist)))
        ;; This is the part from the original imenu code
        ;; that puts submenus at the top.  huh? why?
        ;; --------------------------------------------
        ;; (setq tail menulist)
        ;; (dolist (item tail)
        ;;   (when (imenu--subalist-p item)
        ;;     (push item keep-at-top)
        ;;     (setq menulist (delq item menulist))))
        (if imenu-sort-function
            (setq menulist (sort menulist imenu-sort-function)))
        (if (> (length menulist) imenu-max-items)
            (setq menulist
                  (mapcar
                   (lambda (menu)
                     (cons (format "From: %s" (caar menu)) menu))
                   (imenu--split menulist imenu-max-items))))
        (setq ad-return-value
              (cons title
                    (nconc (nreverse keep-at-top) menulist))))
    ;; else
    ad-do-it))


;;
;; I used this to examine the performance of the imenu scanning.
;; It's not necessary during normal operation.
;;
;; (defun csharp-imenu-begin-profile ()
;;   "turn on profiling"
;;   (interactive)
;;   (let ((fns '(csharp--on-class-open-curly-p
;;              csharp--on-namespace-open-curly-p
;;              csharp--on-ctor-open-curly-p
;;              csharp--on-enum-open-curly-p
;;              csharp--on-intf-open-curly-p
;;              csharp--on-prop-open-curly-p
;;              csharp--on-indexer-open-curly-p
;;              csharp--on-defun-open-curly-p
;;              csharp--imenu-create-index-helper
;;              looking-back
;;              looking-at)))
;;     (if (fboundp 'elp-reset-all)
;;         (elp-reset-all))
;;     (mapc 'elp-instrument-function fns)))



(defun csharp--imenu-remove-param-names-from-paramlist (s)
  "The input string S is a parameter list, of the form seen in a
C# method.  TYPE1 NAME1 [, TYPE2 NAME2 ...]

This fn returns a string of the form TYPE1 [, TYPE2...]

Upon entry, it's assumed that the parens included in S.

"
  (if (string= s "()")
      s
    (save-match-data
      (let* (new
             (state 0)  ;; 0 => ws, 1=>slurping param...
             c
             cs
             quoting
             nesting
             need-type
             ix2
             (s2 (substring s 1 -1))
             (len (length s2))
             (i (1- len)))

        (while (> i 0)
          (setq c (aref s2 i) ;; current character
                cs (char-to-string c)) ;; s.t. as a string

          (cond

           ;; backing over whitespace "after" the param
           ((= state 0)
            (cond
             ;; more ws. = is equal to whitespace in the sense that its follows a param-name.
             ((string-match "[ \t\f\v\n\r=]" cs)
              t)
             ((string-match "[\"']" cs)
              ;; a quote means we're probably dealing with a stringy default-value
              ;; back out until we're back into unquoted context
              (setq quoting cs
                    state 5))
             ;; a legal char for an identifier
             ((string-match "[A-Za-z_0-9]" cs)
              (setq state 1))
             (t
              (error "unexpected char (A)"))))

           ;; slurping param name
           ((= state 1)
            (cond
             ;; ws signifies the end of the param
             ((string-match "[ \t\f\v\n\r]" cs)
              (setq state 2))
             ((string-match "[=]" cs)
              ;; = means what we slurped was a default-value for a param
              ;; go back to slurping param-name
              (setq state 0))
             ;; a legal char for an identifier
             ;; (or . for object-access in default value)
             ((string-match "[A-Za-z_0-9\.]" cs)
              t)
             (t
              (error "unexpected char (B)"))))


           ;; ws between typespec and param name
           ((= state 2)
            (cond
             ((string-match "[ \t\f\v\n\r]" cs)
              t)
             ((string-match "[=]" cs)
              ;; = means what we slurped was a default-value for a param
              ;; go back to slurping param-name
              (setq state 0))
             ;; non-ws indicates the type spec is beginning
             (t
              (cl-incf i)
              (setq state 3
                    need-type nil
                    nesting 0
                    ix2 i))))
           ;; slurping type
           ((= state 3)
            (cond
             ((= ?> c) (cl-incf nesting))
             ((= ?< c)
              (cl-decf nesting)
              (setq need-type t))

             ;; ws or comma maybe signifies the end of the typespec
             ((string-match "[ \t\f\v\n\r,]" cs)
              (if (and (= nesting 0) (not need-type))
                  (progn
                    (setq new (cons (substring s2 (1+ i) ix2) new))
                    (setq state
                          (if (= c ?,) 0 4)))))

             ((string-match "[A-Za-z_0-9]" cs)
              (setq need-type nil))))

           ;; awaiting comma or b-o-s
           ((= state 4)
            (cond
             ((= ?, c)
              (if  (= nesting 0)
                  (setq state 0)))
             ((string-match "[ \t\f\v\n\r]" cs)
              t)
             ((= 93 c) (cl-incf nesting)) ;; sq brack
             ((= 91 c)  ;; open sq brack
              (cl-decf nesting))
             ;; handle this (extension methods), out, ref, params
             ((and (>= i 5)
                   (string= (substring s2 (- i 5) (1+ i)) "params"))
              (setf (car new) (concat "params " (car new)))
              (setq i (- i 5)))
             ((and (>= i 3)
                   (string= (substring s2 (- i 3) (1+ i)) "this"))
              (setf (car new) (concat "this " (car new)))
              (setq i (- i 3)))
             ((and (>= i 2)
                   (string= (substring s2 (- i 2) (1+ i)) "ref"))
              (setf (car new) (concat "ref " (car new)))
              (setq i (- i 2)))
             ((and (>= i 2)
                   (string= (substring s2 (- i 2) (1+ i)) "out"))
              (setf (car new) (concat "out " (car new)))
              (setq i (- i 2)))
             (t
              (error "unexpected char (C)"))))
           ;; in a quoted context of a default-value.
           ;; we're basically waiting for a matching quote, to go back to slurping param-name
           ((= state 5)
            (cond
             ((equal quoting cs)
              ;; we're back to unquoted! slurp param-name!
              (setq state 0))
             (t
              t)))
           )
          (cl-decf i))
        (if (and (= state 3) (= nesting 0))
            (setq new (cons (substring s2 i ix2) new)))
        (concat "("
                (if new
                    (mapconcat 'identity new ", ")
                  "")
                ")")))))
(defun csharp--imenu-item-basic-comparer (a b)
  "Compares the car of each element, assumed to be a string."
  (string-lessp (car a) (car b)))
(defun csharp--imenu-get-method-name-from-sig (sig)
  "Extract a method name with its parameter list from a method
signature, SIG. This is used to aid in sorting methods by name,
and secondarily by parameter list.
 
For this input:

    private Dict<String, int>  DoSomething(int, string)

...the output is:

   DoSomething(int, string)

"
  (let* (c
         result
         (state 0)
         (len (length sig))
         (i (1- len)))
    (while (> i 0)
      (setq c (aref sig i))

      (cond
       ((and (= state 0) (= c 40))
        (setq state 1))

       ((and (= state 1) (or (= c 9) (= c 32)))
        (setq result (substring sig (1+ i))
              i 0)))
      (cl-decf i))
    result))



(defun csharp--imenu-item-method-name-comparer (a b)
  "Compares the method names in the respective cars of each element.

The car of each element is assumed to be a string with multiple
tokens in it, representing a method signature, including access
modifier, return type, and parameter list (surrounded by parens).
If the method takes no params, then it's just an empty pair of
parens.

This fn extracts the method name and param list from that
signature and compares *that*.

"
  (let ((methoda (csharp--imenu-get-method-name-from-sig (car a)))
        (methodb (csharp--imenu-get-method-name-from-sig (car b))))
    ;;(csharp-log -1 "compare '%s' <> '%s'" methoda methodb)
    (string-lessp methoda methodb)))



(defun csharp--imenu-create-index-helper (&optional parent-ns indent-level
                                                    consider-usings consider-namespaces)
  "Helper fn for `csharp-imenu-create-index'.

Scans a possibly narrowed section of a c# buffer.  It finds
namespaces, classes, structs, enums, interfaces, and methods
within classes and structs.

The way it works: it looks for an open-curly.  If the open-curly
is a namespace or a class, it narrows to whatever is inside the
curlies, then recurses.

Otherwise (the open-curly is neither of those things), this fn
tries to recognize the open-curly as the beginning of an enum,
method, or interface.

If it succeeds, then a menu item is created for the thing. Then
it jumps to the matching close-curly, and continues. Stop when no
more open-curlies are found.

"

  ;; A C# module consists of zero of more explicitly denoted (and
  ;; possibly nested) namespaces. In the absence of an
  ;; explicitly-denoted namespace, the global namespace is implicitly
  ;; applied.  Within each namespace there can be zero or more
  ;; "container" things - like class, struct, or interface; each with
  ;; zero or more indexable items - like methods, constructors.
  ;; and so on.

  ;; This fn parses the module and indexes those items, creating a
  ;; hierarchically organized list to describe them.  Each container
  ;; (ns/class/struct/etc) is represented on a separate submenu.

  ;; It works like this:
  ;; (start at the top of the module)
  ;;
  ;; 1. look for a using clause
  ;;    yes - insert an item in the menu; move past all using clauses.
  ;;
  ;; 2. go to next open curly
  ;;
  ;; 2. beginning of a container? (a class or namespace)
  ;;
  ;;    yes - narrow, and recurse
  ;;
  ;;    no - create a menu item for the thing, whatever it is.  add to
  ;;         the submenu. Go to the end of the thing (to the matching
  ;;         close curly) then goto step 1.
  ;;

  (let (container-name
        (pos-last-curly -1)
        this-flavor
        this-item
        this-menu
        found-usings
        done)

    (while (not done)

      ;; move to the next thing
      (c-forward-syntactic-ws)
      (cond
       ((and consider-usings
             (re-search-forward (csharp--regexp 'using-stmt) (point-max) t))
        (goto-char (match-beginning 1))
        (setq found-usings t
              done nil))

       ((re-search-forward "{" (point-max) t)
        (if (= pos-last-curly (point))
            (progn
              ;;(csharp-log -1 "imenu: No advance? quitting (%d)" (point))
              (setq done t)) ;; haven't advanced- likely a loop

          (setq pos-last-curly (point))
          (let ((literal (csharp-in-literal)))
            ;; skip over comments?
            (cond

             ((memq literal '(c c++))
              (while (memq literal '(c c++))
                (end-of-line)
                (forward-char 1)
                (setq literal (csharp-in-literal)))
              (if (re-search-forward "{" (point-max) t)
                  (forward-char -1)
                ;;(csharp-log -1 "imenu: No more curlies (A) (%d)" (point))
                (setq done t)))

             ((eq literal 'string)
              (if  (re-search-forward "\"" (point-max) t)
                  (forward-char 1)
                ;;(csharp-log -1 "imenu: Never-ending string? posn(%d)" (point))
                (setq done t)))

             (t
              (forward-char -1)))))) ;; backup onto the curly

       (t
        ;;(csharp-log -1 "imenu: No more curlies (B) posn(%d)" (point))
        (setq done t)))


      (if (not done)
          (cond

           ;; case 1: open curly for an array initializer
           ((looking-back "\\[\\][ \t\n\r]*" nil)
            (forward-sexp 1))

           ;; case 2: just jumped over a string
           ((looking-back "\"" nil)
            (forward-char 1))

           ;; case 3: at the head of a block of using statements
           (found-usings
            (setq found-usings nil
                  consider-usings nil) ;; only one batch
            (let ((first-using (match-beginning 1))
                  (count 0)
                  marquis
                  ;; don't search beyond next open curly
                  (limit (1-
                          (save-excursion
                            (re-search-forward "{" (point-max) t)))))

              ;; count the using statements
              (while (re-search-forward (csharp--regexp 'using-stmt) limit t)
                (cl-incf count))

              (setq marquis (if (eq count 1) "using (1)"
                              (format "usings (%d)" count)))
              (push (cons marquis first-using) this-menu)))


           ;; case 4: an interface or enum inside the container
           ;; (must come before class / namespace )
           ((or (csharp--on-intf-open-curly-p)
                (csharp--on-enum-open-curly-p))
            (setq consider-namespaces nil
                  consider-usings nil
                  container-name (if parent-ns
                                     (concat parent-ns ".")
                                   nil)
                  this-menu (append this-menu
                                    (list
                                     (cons (concat
                                            (match-string-no-properties 1) ;; thing flavor
                                            " "
                                            container-name
                                            (match-string-no-properties 2)) ;; intf name
                                           (match-beginning 1)))))
            (forward-sexp 1))


           ;; case 5: at the start of a container (class, namespace)
           ((or (and consider-namespaces (csharp--on-namespace-open-curly-p))
                (csharp--on-class-open-curly-p)
                (csharp--on-genclass-open-curly-p))

            ;; produce a fully-qualified name for this thing
            (if (string= (match-string-no-properties 1) "namespace")
                (setq this-flavor (match-string-no-properties 1)
                      this-item (match-string-no-properties 2))
              (setq this-flavor (match-string-no-properties 2)
                    this-item (match-string-no-properties 3)
                    consider-usings nil
                    consider-namespaces nil))

            (setq container-name (if parent-ns
                                     (concat parent-ns "." this-item)
                                   this-item))

            ;; create a submenu
            (let (submenu
                  (top (match-beginning 1))
                  (open-curly (point))
                  (close-curly (save-excursion
                                 (forward-sexp 1)
                                 (point))))
              (setq submenu
                    (list
                     (concat this-flavor " " container-name)
                     (cons "(top)" top)))

              ;; find all contained items
              (save-restriction
                (narrow-to-region (1+ open-curly) (1- close-curly))

                (let* ((yok (string= this-flavor "namespace"))
                       (child-menu
                        (csharp--imenu-create-index-helper container-name
                                                           (concat indent-level "  ")
                                                           yok yok)))
                  (if child-menu
                      (setq submenu
                            (append submenu
                                    (sort child-menu
                                          'csharp--imenu-item-basic-comparer))))))
              (setq submenu
                    (append submenu
                            (list (cons "(bottom)" close-curly))))

              (setq this-menu
                    (append this-menu (list submenu)))

              (goto-char close-curly)))


           ;; case 6: a property
           ((csharp--on-prop-open-curly-p)
            (setq consider-namespaces nil
                  consider-usings nil
                  this-menu
                  (append this-menu
                          (list
                           (cons (concat
                                  "prop "
                                  (match-string-no-properties 3)) ;; prop name
                                 (match-beginning 1)))))
            (forward-sexp 1))


           ;; case 7: an indexer
           ((csharp--on-indexer-open-curly-p)
            (setq consider-namespaces nil
                  consider-usings nil
                  this-menu
                  (append this-menu
                          (list
                           (cons (concat
                                  "indexer "
                                  (match-string-no-properties 4)) ;; index type
                                 (match-beginning 1)))))
            (forward-sexp 1))


           ;; case 8: a constructor inside the container
           ((csharp--on-ctor-open-curly-p)
            (setq consider-namespaces nil
                  consider-usings nil
                  this-menu
                  (append this-menu
                          (list
                           (cons (concat
                                  "ctor "
                                  (match-string-no-properties 2) ;; ctor name
                                  (csharp--imenu-remove-param-names-from-paramlist
                                   (match-string-no-properties 3))) ;; ctor params
                                 (match-beginning 1)))))
            (forward-sexp 1))


           ;; case 9: a method inside the container
           ((csharp--on-defun-open-curly-p)
            (setq consider-namespaces nil
                  consider-usings nil
                  this-menu
                  (append this-menu
                          (list
                           (cons (concat
                                  "method "
                                  (match-string-no-properties 2) ;; return type
                                  " "
                                  (match-string-no-properties 3) ;; func name
                                  (csharp--imenu-remove-param-names-from-paramlist
                                   (match-string-no-properties 4))) ;; fn params
                                 (match-beginning 1)))))
            (forward-sexp 1))


           ;; case 10: unknown open curly - just jump over it.
           ((looking-at "{")
            (forward-sexp 1))

           ;; case 11: none of the above. shouldn't happen?
           (t
            (forward-char 1)))))

    this-menu))
(defcustom csharp-imenu-max-similar-items-before-extraction 6
  "The maximum number of things of a particular
category (constructor, property, method, etc) that will be
separely displayed on an imenu without factoring them into a
separate submenu.

For example, if a module has 3 consructors, 5 methods, and 7
properties, and the value of this variable is 4, then upon
refactoring, the constructors will remain in the toplevel imenu
and the methods and properties will each get their own
category-specific submenu.
 
See also `csharp-imenu-min-size-for-sub-submenu'.
 
For more information on how csharp-mode uses imenu,
see `csharp-want-imenu', and `csharp-mode'.
"
  :type 'integer
  :group 'csharp)


(defcustom csharp-imenu-min-size-for-sub-submenu 18
  "The minimum number of imenu items  of a particular
category (constructor, property, method, etc) that will be
broken out into sub-submenus.
 
For example, if a module has 28 properties, then the properties will
be placed in a submenu, and then that submenu with be further divided
into smaller submenus.
 
See also `csharp-imenu-max-similar-items-before-extraction'
 
For more information on how csharp-mode uses imenu,
see `csharp-want-imenu', and `csharp-mode'.
"
  :type 'integer
  :group 'csharp)


(defun csharp--first-word (s)
  "gets the first word from the given string.
It had better be a string!"
  (car (split-string s nil t)))


(defun csharp--make-plural (s)
  "make a word plural. For use within the generated imenu."
  (cond
   ((string= s "prop") "properties")
   ((string= s "class") "classes")
   ((string= s "ctor") "constructors")
   (t (concat s "s"))))


(defun csharp--imenu-counts (list)
  "Returns an alist, each item is a cons cell where the car is a
unique first substring of an element of LIST, and the cdr is the
number of occurrences of that substring in elements in the
list.
 
For a complicated imenu generated for a large C# module, the result of
this fn will be something like this:
 
    ((\"(top)\"        . 1)
     (\"properties\"   . 38)
     (\"methods\"      . 12)
     (\"constructors\" . 7)
     (\"(bottom)\"     . 1))
 
"
  (letrec ((helper
            (lambda (list new)
              (if (null list) new
                (let* ((elt (car list))
                       (topic (csharp--make-plural
                               (csharp--first-word(car elt))))
                       (xelt (assoc topic new)))
                  (funcall helper (cdr list)
                           (if xelt
                               (progn (cl-incf (cdr xelt)) new)
                             (cons (cons topic 1) new))))))))
    (nreverse (funcall helper list nil))))



(defun csharp--imenu-get-submenu-size (n)
  "Gets the preferred size of submenus given N, the size of the
flat, unparceled menu.

Suppose there are 50 properties in a given C# module. This fn maps
from that number, to the maximum size of the submenus into which the
large set of properties should be broken.

Currently the submenu size for 50 is 12.  To change this, change
the lookup table.

The reason it's a lookup table and not a simple arithmetic
function: I think it would look silly to have 2 submenus each
with 24 items.  Sixteen or 18 items on a submenu seems fine when
you're working through 120 items total. But if you have only 28
items, better to have 3 submenus with 10 and 9 items each.  So
it's not a linear function. That's what this lookup tries to do.
 
"
  (let ((size-pairs '((100 . 22)
                      (80 . 20)
                      (60 . 18)
                      (40 . 15)
                      (30 . 14)
                      (24 . 11)
                      (0  . 9)))
        elt
        (r 0))

    (while (and size-pairs (eq r 0))
      (setq elt (car size-pairs))
      (if (> n (car elt))
          (setq r (cdr elt)))
      (setq size-pairs (cdr size-pairs)))
    r))



(defun csharp--imenu-remove-category-names (menu-list)
  "Input is a list, each element is (LABEL . LOCATION). This fn
returns a modified list, with the first word - the category name
- removed from each label.
 
"
  (mapcar (lambda (elt)
            (let ((tokens (split-string (car elt) "[ \t]" t)))
              (cons (mapconcat 'identity (cdr tokens) " ")
                    (cdr elt))))
          menu-list))

(defun csharp--imenu-submenu-label (sig flavor)
  "generate a submenu label from the given signature, SIG.
The sig is a method signature, property type-and-name,
constructor, and so on, indicated by FLAVOR.
 
This fn returns a simple name that can be used in the label for a
break out submenu.
 
"
  (if (string= flavor "method")
      (let ((method-name (csharp--imenu-get-method-name-from-sig sig)))
        (substring method-name 0 (string-indexof method-name 40)))
    (substring sig (1+ (string-lastindexof sig 32)))))




(defun csharp--imenu-break-one-menu-into-submenus (menu-list)
  "Parcels a flat list MENU-LIST up into smaller sublists. It tries
to balance the number of sublists and the size of each sublist.
 
The max size of any sublist will be about 20 (arbitrary) and the
min size will be 7 or so. See `csharp--imenu-get-submenu-size'
for how this is done.
 
It does this destructively, using `nbutlast'.
 
Returns a new list, containing sublists.
"
  
  (let ((len (length menu-list))
        (counts (csharp--imenu-counts menu-list)))
    
    (cond
     ;; a small number, and all the same flavor
     ((and (< len csharp-imenu-min-size-for-sub-submenu) (= (length counts) 1))
      (csharp--imenu-remove-category-names
       (sort menu-list
             (if (string= (caar counts) "methods")
                 'csharp--imenu-item-method-name-comparer
               'csharp--imenu-item-basic-comparer))))
     
     ;; is the length already pretty short?
     ((< len csharp-imenu-min-size-for-sub-submenu)
      menu-list)
     
     ((/= (length counts) 1)
      menu-list)
     
     (t
      (let* ((lst    (sort menu-list
                           (if (string= (caar counts) "methods")
                               'csharp--imenu-item-method-name-comparer
                             'csharp--imenu-item-basic-comparer)))
             new
             (sz     (csharp--imenu-get-submenu-size len)) ;; goal max size of sublist
             (n      (ceiling (/ (* 1.0 len) sz))) ;; total number of sublists
             (adj-sz (ceiling (/ (* 1.0 len) n)))  ;; maybe a little less than sz
             (nsmall (mod (- adj-sz (mod len adj-sz)) adj-sz)) ;; num of (n-1) lists
             (i      0)
             (base-name (csharp--first-word (caar lst)))
             label
             chunksz
             this-chunk)

        (while lst
          (setq chunksz (if (> nsmall i) (1- adj-sz) adj-sz)
                this-chunk (csharp--imenu-remove-category-names
                            (nthcdr (- len chunksz) lst))
                lst (nbutlast lst chunksz)
                ;;label (format "%s %d" plural-name (- n i))
                label (concat "from " (csharp--imenu-submenu-label (caar this-chunk) base-name))
                new (cons (cons label this-chunk) new)
                len (- len chunksz))
          (cl-incf i))
        new)))))



(defun csharp--imenu-break-into-submenus (menu-list)
  "For an imenu menu-list with category-based submenus,
possibly break a submenu into smaller sublists, based on size.
 
"
  (mapcar (lambda (elt)
            (if (imenu--subalist-p elt)
                (cons (car elt)
                      (csharp--imenu-break-one-menu-into-submenus (cdr elt)))
              elt))
          menu-list))





(defun csharp--imenu-reorg-alist-intelligently (menu-alist)
  "Accepts an imenu alist. Returns an alist, reorganized.
Things get sorted, factored out into category submenus,
and split into multiple submenus, where conditions warrant.

For example, suppose this imenu alist is generated from a scan:

    ((\"usings (4)\" . 1538)
     (\"namespace Ionic.Zip\"
      (\"(top)\" . 1651)
      (\"partial class Ionic.Zip.ZipFile\"
       (\"(top)\" . 5473)
       (\"prop FullScan\" . 8036)
           ...
       (\"prop Comment\" . 21118)
       (\"prop Verbose\" . 32278)
       (\"method override String ToString\" . 96577)
       (\"method internal void NotifyEntryChanged\" . 97608)
          ....
       (\"method internal void Reset\" . 98231)
       (\"ctor ZipFile\" . 103598)
           ...
       (\"ctor ZipFile\" . 109723)
       (\"ctor ZipFile\" . 116487)
       (\"indexer int\" . 121232)
       (\"indexer String\" . 124933)
       (\"(bottom)\" . 149777))
      (\"public enum Zip64Option\" . 153839)
      (\"enum AddOrUpdateAction\" . 154815)
      (\"(bottom)\" . 154893)))


This is displayed as a toplevel menu with 2 items; the namespace
menu has 5 items (top, bottom, the 2 enums, and the class).  The
class menu has 93 items. It needs to be reorganized to be more usable.

After transformation of the alist through this fn, the result is:

    ((\"usings (4)\" . 1538)
     (\"namespace Ionic.Zip\"
      (\"(top)\" . 1651)
      (\"partial class Ionic.Zip.ZipFile\"
       (\"(top)\" . 5473)
       (\"properties\"
        (\"WriteStream\" . 146489)
        (\"Count\" . 133827)
            ....
        (\"BufferSize\" . 12837)
        (\"FullScan\" . 8036))
       (\"methods\"
        (\"virtual void Dispose\" . 144389)
        (\"void RemoveEntry\" . 141027)
           ....
        (\"method override String ToString\" . 96577)
        (\"method bool ContainsEntry\" . 32517))
       (\"constructors\"
        (\"ZipFile\" . 116487)
           ....
        (\"ZipFile\" . 105698)
        (\"ZipFile\" . 103598))
       (\"indexer int\" . 121232)
       (\"indexer String\" . 124933)
       (\"(bottom)\" . 149777))
      (\"public enum Zip64Option\" . 153839)
      (\"enum AddOrUpdateAction\" . 154815)
      (\"(bottom)\" . 154893)))

All menus are the same except the class menu, which has been
organized into subtopics, each of which gets its own cascaded
submenu.  If the submenu itself holds more than
`csharp-imenu-max-similar-items-before-extraction' items that are
all the same flavor (properties, methods, etc), thos get split
out into multiple submenus.
 
"
  (let ((counts (csharp--imenu-counts menu-alist)))
    (letrec ((helper
              (lambda (list new)
                (if (null list)
                    new
                  (let* ((elt (car list))
                         (topic (csharp--make-plural
                                 (csharp--first-word (car elt))))
                         (xelt (assoc topic new)))
                    (funcall
                     helper (cdr list)
                     (if xelt
                         (progn
                           (rplacd xelt (cons elt (cdr xelt)))
                           new)
                       (cons

                        (cond
                         ((> (cdr (assoc topic counts))
                             csharp-imenu-max-similar-items-before-extraction)
                          (cons topic (list elt)))

                         ((imenu--subalist-p elt)
                          (cons (car elt)
                                (csharp--imenu-reorg-alist-intelligently (cdr elt))))
                         (t
                          elt))

                        new))))))))

      (csharp--imenu-break-into-submenus
       (nreverse (funcall helper menu-alist nil))))))




(defun csharp-imenu-create-index ()
  "This function is called by imenu to create an index for the
current C# buffer, conforming to the format specified in
`imenu--index-alist' .

See `imenu-create-index-function' for background information.

To produce the index, which lists the classes, functions,
methods, and properties for the current buffer, this function
scans the entire buffer.

This can take a long time for a large buffer. The scan uses
regular expressions that attempt to match on the general-case C#
syntax, for classes and functions, generic types, base-classes,
implemented interfaces, and so on. This can be time-consuming.
For a large source file, say 160k, it can take 10 seconds or more.
The UI hangs during the scan.

imenu calls this fn when it feels like it, I suppose when it
thinks the buffer has been updated. The user can also kick it off
explicitly by selecting *Rescan* from the imenu menu.

After generating the hierarchical list of props, methods,
interfaces, classes, and namespaces, csharp-mode re-organizes the
list as appropriate:

 - it extracts sets of like items into submenus. All properties
   will be placed on a submenu. See
   `csharp-imenu-max-similar-items-before-extraction' for a way
   to tune this.

 - it converts those submenus into sub-submenus, if there are more than
   `csharp-imenu-min-size-for-sub-submenu' items.

 - it sorts each set of items on the outermost menus lexicographically.

The result of these transformations is what is provided to imenu
to generate the visible menus.  Just FYI - the reorganization of
the scan results is much much faster than the actual generation
of the scan results. If you're looking to save time, the re-org
logic is not where the cost is.

imenu itself likes to sort the menus. See `imenu--split-menu' and
also `csharp--imenu-split-menu-patch', which is advice that
attempts to disable the weird re-jiggering that imenu performs.
 
"
  ;; I think widen/narrow causes the buffer to be marked as
  ;; modified. This is a bit surprising, but I have no other
  ;; explanation for the source of the problem.
  ;; So I use `c-save-buffer-state' so that the buffer is not
  ;; marked modified when the scan completes.
  
  (c-save-buffer-state ()
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))

        (let ((index-alist
               (csharp--imenu-create-index-helper nil "" t t)))

          (csharp--imenu-reorg-alist-intelligently index-alist)

          ;;index-alist

          ;; What follows is No longer used.
          ;; =======================================================

          ;; If the index menu contains exactly one element, and it is
          ;; a namespace menu, then remove it.  This simplifies the
          ;; menu, and results in no loss of information: all types
          ;; get fully-qualified names anyway. This will probably
          ;; cover the majority of cases; often a C# source module
          ;; defines either one class, or a set of related classes
          ;; inside a single namespace.

          ;; To remove that namespace, we need to prune & graft the tree.
          ;; Remove the ns hierarchy level, but also remove the 1st and
          ;; last elements in the sub-menu, which represent the top and
          ;; bottom of the namespace.

          ;; (if (and
          ;;      (= 1 (length index-alist))
          ;;      (consp (car index-alist))
          ;;      (let ((tokens (split-string
          ;;                     (car (car index-alist))
          ;;                     "[ \t]" t)))
          ;;        (and (<= 1 (length tokens))
          ;;             (string= (downcase
          ;;                       (nth 0 tokens)) "namespace"))))
          ;;
          ;;     (let (elt
          ;;           (newlist (cdar index-alist)))
          ;;       (setf (car (car newlist))  (car (car index-alist)))
          ;;       newlist)
          ;;
          ;;   index-alist)

          )))))

