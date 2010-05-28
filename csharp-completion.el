;;; csharp-completion.el -- Smart code completion for C#
;;
;; Author:     Dino Chiesa <dpchiesa@hotmail.com>
;; Maintainer: Dino Chiesa <dpchiesa@hotmail.com>
;; Created:    April 2010
;; Modified:   April 2010
;; Version:    0.1.5
;; Keywords:   c# languages oop mode
;; X-URL:      http://code.google.com/p/csharpmode/
;;
;;
;;; Commentary:
;;
;;    This is a code-completion or "intellisense" package for C#.  The
;;    scope of this module is much smaller that a full "development
;;    envvironment".  It does smart code completion for C#, in
;;    emacs. It does not do font-lock, indenting, debugging,
;;    compiling, profiling, and so on.
;;
;;    To use it, place the cursor after a partially-completed
;;    statement, and invoke `cscomp-complete-at-point'.  Normally this
;;    would be bound to a particular keystroke, like M-.  This module
;;    will insert the first completion that matches. If multiple
;;    completions are possible, calling the completion function again
;;    will cycle through the possibilities, similar to the way
;;    dabbrev-mode works.
;;
;;    You can also call `cscomp-complete-at-point-menu', and get a popup
;;    menu of the completion choices.
;;
;;    There are 2 complementary sources of information for the
;;    completions: introspection into compiled .NET class libraries
;;    (like the base class library), and parse analysis from the
;;    semantic.el package, which is part of CEDET. In the typical
;;    case, this module uses both of those sources of information,
;;    together.
;;
;;    The reflection is done by an inferior powershell shell running
;;    within emacs, that has loaded a custom .NET assembly.  The
;;    library exposes static methods that perform type reflection,
;;    using the capabilities of the System.Reflection namespace. These
;;    methods then return strings which are lisp s-expressions that
;;    can be eval'd, resulting in structures containing information
;;    for a given type - including the available methods, fields and
;;    properties, and the attributes on same.  This piece of the
;;    puzzle is called the "CscompShell".
;;
;;    As an example, suppose your code has a local variable of type
;;    System.Xml.XmlDocument, named doc.  Suppose the user asks for
;;    completion on that variable.  The module uses the semantic parse
;;    data to identify the name and type of the local variable.  It then
;;    sends a "GetTypeInfo" command to the CscompShell, passing
;;    System.Xml.XmlDocument. The CscompShell returns an s-expression
;;    enumerating the fields, methods and properties for that type.
;;    This s-expression is then used to populate the completion list.
;;
;;
;; Here's a survey of the situations in which this module can offer
;; completions:
;;
;;    a. names of local variables, instance variables, and method arguments.
;;
;;         void Method1(String longArgumentName)
;;         {
;;            long?
;;         }
;;
;;    b. Methods, fields and properties (m/f/p) on a local variable
;;       with known type, on method arguments of known type, or on
;;       instance variables of known type:
;;
;;         String x = "this is a string";
;;         x.?
;;
;;    c. M/f/p on local variables with var type:
;;
;;         var x = "this is a string";
;;         x.?
;;
;;    d. Cascading local variable declarations of var type:
;;
;;         var s = "This is a string";
;;         var length = s.Length;
;;         var radix = length.?
;;
;;    e. completion on generic types:
;;
;;         var x = new List<String>();
;;         x.?
;;
;;    f. completion on local variables that are initialized
;;       from instance methods and variables.
;;
;;         void method1()
;;         {
;;           var length = this.InstanceMethod();
;;           length.?
;;         }
;;
;;    g. constructor completion, provide template when completing
;;
;;         var x = new System.String(?
;;
;;    h. constructor completion as above, with unqualified type.
;;
;;         var x = new TimeSpan(?
;;
;;    i. finding constructors among qualified and unqualified types
;;
;;         var x = new TimeS?
;;
;;    j. complete static methods on known types,
;;       whether fully qualified or unqualified.
;;
;;         String.Jo?
;;
;;    k. present template fpr static methods on known types,
;;       whether fully qualified or unqualified.
;;
;;         String.Join(?
;;
;;    l. Immediate values.
;;
;;         7.C?
;;
;;    m. other compound Expressions:
;;
;;         Path.GetRandomFileName().Normalize().?
;;
;;
;;
;; =======================================================
;;
;; Dependencies:
;;
;;   cc-mode 5.31.?
;;
;;   semantic.el 1.0pre7
;;
;;   optionally, yasnippet, for snippet insertion.  If the user has
;;     yasnippet loaded, then this completion module will insert
;;     a snippet (template) when the user selects a Method from the
;;     completion list menu.  The method snippet will have all the
;;     method parameters as placeholders; the developer then types over
;;     those placeholders to supply the actual method parameters.
;;     If yasnippet is not loaded, then the completion is just
;;     the method name, and the developer has to fill in the
;;     param-list himself.
;;
;;   PowerShell, and a separate DLL that must run in Powershell.  That
;;     DLL is implemented in C#.
;;
;;
;;
;;
;; Known bugs/problems :
;;
;;    1. The module does not do completion on anonymous (var) types in
;;       for loops.
;;
;;
;;
;; TODO :
;;
;;    make an installer.
;;
;; Please send any comments, bugs, or upgrade requests to
;; Dino Chiesa (dpchiesa@hotmail.com)
;;


(require 'csharp-shell)

(require 'semantic-idle)  ;; for ... reparsing a buffer


;; Design notes:
;;
;; Tue, 04 May 2010  10:47
;;
;; This completion depends on the semantic package for parsing a C#
;; buffer.  That gives the module a way to interrogate the names and
;; types of local and instance variables, in order to do completion on
;; them.
;;
;; Semantic also provides the list of using statements for a C# module,
;; which tells us which assemblies we need to search in, for
;; completions.  These are then loaded into CscompShell for interrogation.
;;



(defvar cscomp-current-list nil
  "The list of all the completion. Each element of the list is
either a string, or a list which the car is the possible completion,
and the cadr is an additional information about this completion.")

(defvar cscomp-current-list-index nil
  "An index to an element in cscomp-current-list. This is used to
cycle the list.")

(defvar cscomp-current-fragment nil
  "The current fragment we're trying to complete. This is used to trim the thing that gets inserted.")

(defvar cscomp-current-beginning (make-marker)
  "The beginning of the region where the last completion was inserted.")

(defvar cscomp-current-end (make-marker)
  "The end of the region where the last completion was inserted.")

(defvar cscomp-typeinfo-cache nil)

(defcustom cscomp-typeinfo-cache-size 150
  "The max size of completion buffer cache, in entries."
  :group 'cscomp
  :type 'integer)



(defun cscomp-referenced-assemblies-list ()
  "Return the list of .NET namespaces referenced in the
current buffer via using statements. It uses the semantic parser
table to find the 'using' statements. "
  (interactive)
  (if (not (semantic-active-p)) (semantic-new-buffer-fcn))
  (let* ((tokens  (semantic-fetch-tags))
         ;;(usings (semantic-find-nonterminal-by-token (quote include) tokens))
         (usings (semantic-brute-find-tag-by-class 'include tokens)))
    (cscomp-log 3 "cscomp-referenced-assemblies-list: found using statements: '%s'" usings)
    (mapcar 'car usings)))



(defun cscomp-instance-vars ()
  "Return the list of instance variables in a C# module.
This uses the semantic parser table to find the variable
declarations.

The return value is a list of semantic tags.  Looks like:

\((\"flavor\" variable
             (:type \"int\")
             (reparse-symbol class_member_declaration) #<overlay from 580 to 595 in a.cs>)
 (\"flicky\" variable
             (:type \"String\")
             (reparse-symbol class_member_declaration) #<overlay from 604 to 636 in a.cs>)
 (\"label\" variable
            (:type \"String\")
            (reparse-symbol class_member_declaration) #<overlay from 645 to 669 in a.cs>))

"

  (if (not (semantic-active-p)) (semantic-new-buffer-fcn))
  (semantic-fetch-tags)
  (let* ((class  (cscomp-find-enclosing-csharp-class))        ;; the enclosing class
         (tokens (semantic-tag-type-members class))                  ;; the members of that class
         (vars (semantic-brute-find-tag-by-class 'variable tokens))) ;; the members that are variables
    (cscomp-log 3 "cscomp-instance-vars: found instance vars: '%s'" vars)
    vars))




(defun cscomp-instance-members ()
  "Return the list of instance members in a C# module.
This uses the semantic parser table to find the memebr
declarations.

The return value is a list of semantic tags.  Looks like:

\((\"flavor\" variable
             (:type \"int\")
             (reparse-symbol class_member_declaration) #<overlay from 580 to 595 in a.cs>)
 (\"Hello\" function
             (:type \"String\")
             (reparse-symbol class_member_declaration) #<overlay from 604 to 636 in a.cs>)
 (\"label\" variable
            (:type \"String\")
            (reparse-symbol class_member_declaration) #<overlay from 645 to 669 in a.cs>))

"

  (if (not (semantic-active-p)) (semantic-new-buffer-fcn))
  (semantic-fetch-tags)
  (let ((class  (cscomp-find-enclosing-csharp-class))) ;; the enclosing class
    (semantic-tag-type-members class)))                       ;; the members of that class




(defun cscomp--find-matching-tags (name-fragment tagset &optional local)
  "Return the list of tags from a given set, that
match NAME-FRAGMENT.  This is used by `cscomp-matching-local-vars',
`cscomp-matching-instance-vars',
`cscomp-matching-instance-members'.
"
    (let ((var-label (if local "(Variable) " "(Field) " ))
           result)

    (if tagset
        (progn
        (while tagset
          (let* ((tag (car tagset))
                 (member-name  (semantic-tag-name tag))
                 (member-type  (semantic-tag-type tag))
                 (member-clazz (semantic-tag-class tag))
                 )

            (if (eq 0 (string-match name-fragment member-name))
                (let ((descrip
                       (cond
                        ((string= member-clazz "variable")
                         (concat var-label member-type))

                        ((string= member-clazz "function")
                         (let* ((arglist (semantic-tag-get-attribute tag :arguments))
                                (modifiers (semantic-tag-modifiers tag))
                                (arg-descrip
                                 (if (> (length arglist) 0)
                                     (concat "("
                                             (mapconcat
                                              '(lambda (x) (concat (semantic-tag-type x)
                                                                   " "
                                                                   (semantic-tag-name x)))
                                              arglist  ", ")
                                             ")")
                                   "()")))

                           (concat "(Method) "
                                   " " (mapconcat 'identity modifiers " ") " "
                                   arg-descrip
                                   "  returns "
                                   member-type)))

                        (t ""))))


                  (cscomp-log 2 "cscomp-matching-tags: found %s (%s)"
                           member-name member-type)

                  (setq result (cons (list member-name descrip) result)))))

            (setq tagset (cdr tagset)))
        (cscomp-sort-completion-list result))
      nil)))




(defun cscomp-matching-instance-vars (name-fragment)
  "Return the list of instance variables in a C# module, that
match NAME-FRAGMENT.  See also, `cscomp-matching-local-vars'.

"
  (cscomp--find-matching-tags name-fragment (cscomp-instance-vars)))




(defun cscomp-matching-instance-members (name-fragment)
  "Return the list of instance memebrs in a C# module, that
match NAME-FRAGMENT.

See also, `cscomp-matching-local-vars',
`cscomp-matching-instance-vars'.

"
  (cscomp--find-matching-tags name-fragment (cscomp-instance-members)))



(defun cscomp-matching-local-vars (name-fragment)
  "Use the semantic lex/analysis results to find local variables
that match the given NAME-FRAGMENT.

See also, `cscomp-matching-instance-members',
`cscomp-matching-instance-vars'.

"
  (cscomp-start-stripped-semantic)
  (cscomp--find-matching-tags name-fragment (semantic-get-all-local-variables) t))



(defun cscomp-find-enclosing-csharp-class (&optional posn)
  "returns a tag of type 'type (in other words, a c# class or struct) that
encloses POSN, or point if POSN is nil.  If there is no enclosing 'type,
then return nil.
"
;; This isn't quite correct. At this time, the C# namespace defn gets
;; parsed as a type. I couldn't figure how to get namespace to get
;; parsed as a new 'namespace tag.  Therefore, this fn can sometimes
;; return a tag corresponding to a namespace, as opposed to a tag
;; corresponding to a bonafide type.

(let ((nar (semantic-current-tag-of-class 'type)))
    nar))



;; (defun cscomp-find-semantic-things (tag-class)
;;   "Search for tags in semantic parse state."
;;   (interactive "sTag type: ")
;;   (if (not (semantic-active-p)) (semantic-new-buffer-fcn))
;;   (let* ((tokens  (semantic-fetch-tags))
;;          (matches (cscomp-find-by-class tag-class tokens t)))
;;     (if matches
;;         (while matches
;;           (let ((one (car matches)))
;;             (setq matches (cdr matches))))
;;       nil
;;     )))



(defun cscomp-find-by-class (cls streamorbuffer &optional search-parts search-includes)
  "Find all tags with class CLS within STREAMORBUFFER.
CLS is a string which is the name of the class of the tags returned, such as
include, variable, type.
See `semantic-tag-class'.
Optional argument SEARCH-PARTS and SEARCH-INCLUDES are passed to
`semantic-brute-find-tag-by-function'."
  (semantic-brute-find-tag-by-function
   (lambda (tag)
     (let ((class (semantic-tag-class tag)))
       (string= class cls)))
   streamorbuffer search-parts search-includes))



;;    (lambda (tag)
;;      (let ((class (semantic-tag-class tag)))
;;        (if (and (listp ts)
;;                 (or (= (length ts) 1)
;;                     (string= (semantic-tag-class ts) type)))
;;
;;            (setq ts (semantic-tag-name ts)))
;;        (equal type ts)))
;;    streamorbuffer search-parts search-includes))



(defun cscomp-debugonly-list-all-local-variables ()
  "fiddle with local variables and semantic."
  (interactive)
  (cscomp-start-stripped-semantic)
  (semantic-lex (point-min) (point-max) 100)
  (let ((locals (semantic-get-all-local-variables)))
    (mapcar '(lambda (x)
               (cscomp-log 3 "local var: name(%s) type(%s) pos(%s)"
                        (car x) (cadr (nth 2 x))
                        (nth 4 x)))
            locals)))



;; (defun cscomp-variables-in-scope ()
;;   "Return the list of variables currently in scope.
;; It uses the semantic parser table to find them."
;;   (interactive)
;;   (if (not (semantic-active-p)) (semantic-new-buffer-fcn))
;;   (let* ((tokens (semantic-fetch-tags))
;;          (vars (semantic-brute-find-tag-by-class 'variable tokens)))
;;     (cscomp-log 3 "cscomp-variables-in-scope: found variables: '%s'" vars)
;;     (mapcar 'car vars)))


;; (defun cscomp-valid-csharp-declaration-at (point varname)
;;   "Verify that a POINT starts a valid csharp declaration
;; for the VARNAME variable."
;;   (save-excursion
;;     (goto-char point)
;;     (if (looking-at
;;          (concat "\\([A-Za-z0-9_.\177-\377]+\\)[ \t\n\r]+"
;;                  (cscomp-double-backquotes varname)
;;                  "[ \t\n\r]*[;=]"))
;;         (match-string 1)
;;       nil)))




;; (defun cscomp-double-backslashes (varname)
;;   "Build a new string identical to VARNAME, except that every backslash
;; `\' is doubled, so that it can be used in a regex expression.
;; "
;;   (let (result (idx 0) (len (length varname)) curcar)
;;     (while (< idx len)
;;       (setq curcar (elt varname idx))
;;       (setq result (concat result (if (eq curcar ?\\)
;;                                       "\\\\"
;;                                     (make-string 1 curcar))))
;;       (setq idx (1+ idx)))
;;     result))




;; (defun cscomp-declared-type-of (name)
;;   "Find in the current buffer the csharp type of the variable NAME.  The
;; function returns a string containing the name of the class, or nil
;; otherwise. This function does not give the fully-qualified csharp class
;; name, it just returns the type as it is declared."
;;   (save-excursion
;;     (let (found res pos orgpt resname)
;;       (while (and (not found)
;;                   (search-backward name nil t))
;;         (setq pos (point))
;;         (backward-word 1)
;;         (setq resname (cscomp-valid-csharp-declaration-at (point) name))
;;         (goto-char pos)
;;         (forward-char -1)
;;         (if resname
;;             (progn (setq res resname)
;;                    (setq found t))))
;;       res)))


;; (defun cscomp-filter-fqn (importlist)
;;   "Filter all the fully-qualified classnames in the import list. It uses
;; the knowledge that those classnames are at the beginning of the list,
;; so that it can stops at the first package import (with a star `*' at
;; the end of the declaration)."
;;   (if importlist
;;       (if (string= "*" (car (cdr (car importlist))))
;;           importlist
;;         (cscomp-filter-fqn (cdr importlist)))))



(defun cscomp-escape-string-for-powershell (arg)
  "Powershell uses the backquote for an escape char.  This fn
escapes the backquote for a string that will eventually be sent
to powershell (CscompShell).

I think this is only necessary when the arg is submitted to
powershell within double-quotes. If the arg is within single
quotes, backquotes do not need to be escaped.

"
  (let ((matcho (string-match "\\(.+\\)`\\(.+\\)" arg)))
    (if matcho
        (concat
         (substring arg (match-beginning 1) (match-end 1))
         "``"
         (substring arg (match-beginning 2) (match-end 2)))
      arg)))





(defun cscomp-send-string-to-shell (command-string)
  "sends a string to cscompshell function, returns the result."
  (let ((result (csharp-shell-exec-and-eval-result command-string)))
    (cscomp-log 2 "send-string-to-shell (%s) result(%s)" command-string (prin1-to-string  result))
    ;;(if (and result (listp result)) result nil)
    result
    ))



(defun cscomp-invoke-shell-fn (fn arg)
  "invokes a 1-arg CscompShell function, returns the result."

  ;; need to use single-quotes here around the arg, because in
  ;; some cases the arg can have double-quotes in it.

  (let* ((escaped-arg (cscomp-escape-string-for-powershell arg)))
    (cscomp-send-string-to-shell (concat "[Ionic.Cscomp.Utilities]::" fn "(\'" escaped-arg "\')"))))




(defun cscomp-type-exists (typename)
  "Determines if the given type is known by the CscompShell.  You can provide a short type name, or a fully-qualified name.  You must have loaded the assembly into the shell, for it to be known.  See `cscomp-load-assembly'."
  (interactive "sType name: ")
  (cscomp-invoke-shell-fn "QualifyType" typename))



(defun cscomp-get-members-of-class (semantic-tag)
  "Gets the members of the class denoted by the SEMANTIC-TAG.
If SEMANTIC-TAG is nil, then this function gets the closest type
containing point, and gets the members of that.

"
  (if (null semantic-tag) (setq semantic-tag (cscomp-get-current-class)))
  (if (eq (cadr semantic-tag) 'type)
      (semantic-tag-type-members semantic-tag)
    nil))




(defun cscomp-get-current-class (&optional posn)
  "Return the semantic tag for the current class or type in scope at POSN.
"
  (interactive)
  (save-excursion
    (if posn (goto-char posn))
    (semantic-fetch-tags)
    (let ((containing-type (semantic-current-tag-of-class 'type)))
      containing-type)))


(defun cscomp-produce-csharp-arglist-block-from-dbrecord (arglist)
  "Produces an argument list block, suitable for framing within parens
in a method declaration, from ARGLIST, a list of local arguments obtained from
`semantic-get-local-arguments'.

When the format of ARGLIST is like this:

   ((\"count\"
      variable
      (:type \"int\")
      (:filename \"c:/dinoch/dev/dotnet/CsharpCompletion.cs\"
                 reparse-symbol formal_parameters)
      [621 630])
    (\"melvin\"
      variable
      (:type \"string\")
      (:filename \"c:/dinoch/dev/dotnet/CsharpCompletion.cs\"
                 reparse-symbol formal_parameters)
      [631 645]))

The return value is like this:

    int count, string melvin

When the arglist is empty, the return value is a string of zero length.


"
  (let ((fragment ""))
    (while arglist
      (let* ((x (car arglist))
             (var-name (car x))
             (var-type (cadr (nth 2 x))))

        (setq fragment (concat fragment var-type " " var-name)
              arglist (cdr arglist))
        (if arglist
            (setq fragment (concat fragment ", ")))))
    fragment))




(defun cscomp-produce-instance-member-code-fragment (member-list)
  "Produce a C# fragment that defines placeholder instance members,
to be inserted into a class template which is then compiled, so that
Cscomp can inspect the resulting IL to determine the type of a local var
on which the user is asking for completion.

Cscomp uses the compiler to determine the type of the var. It
dynamically generates and compiles a class with the same variable
declaration as the code being edited.

The initialization of the var may depend on instance members.  If
so, the compiled class must provide instance members of the
correct type to satisfy those dependencies.  This function
generates C# code to provide those instance members.

For input that looks like this:

  ((\"staticField1\" variable
     (:typemodifiers (\"private\" \"static\") :type \"int\")
     (reparse-symbol class_member_declaration)
     #<overlay from 605 to 642 in CsharpCompletion.cs>)
   (\"InstanceMethod1\" function
     (:arguments (...) :type \"string\")
     (reparse-symbol class_member_declaration)
     #<overlay from 652 to 741 in CsharpCompletion.cs>)
   (\"Run\" function
     (:typemodifiers (\"public\") :arguments (...) :type \"void\")
     (reparse-symbol class_member_declaration)
     #<overlay from 752 to 1487 in CsharpCompletion.cs>)
   (\"Main\" function
     (:typemodifiers (\"public\" \"static\") :arguments (...) :type \"void\")
     (reparse-symbol class_member_declaration)
     #<overlay from 1497 to 1806 in CsharpCompletion.cs>)
   )

The output will look like this:

    private static int staticField1 = default(int);
    string InstanceMethid(...) { return default(string); }

Any void methods will not be emitted because they cannot affect the
types of local variables declared in methods.

"
  (let ((synthetic-code ""))

    (while member-list
      (let* ((x (car member-list))
             (member-name (car x))
             (member-flavor (cadr x))
             (member-type (semantic-tag-get-attribute x :type))
             (member-modifiers (semantic-tag-get-attribute x :typemodifiers))
             one-frag
             )
        ;;          (message "n(%s) f(%s) t(%s)"
        ;;                   member-name
        ;;                   member-flavor
        ;;                   member-type)

        (setq one-frag
              (cond

               ((string= member-type "void") ;; the member is a void type
                "")                          ;; emit nothing, don't need it.
               ;; it's possible we might need it, in which case,
               ;; we can just emit an empty fn.

               ((eq member-flavor 'function) ;; it's a method
                (concat
                 (mapconcat 'identity member-modifiers " ")
                 " "
                 member-type
                 " "
                 member-name
                 "("
                 (cscomp-produce-csharp-arglist-block-from-dbrecord
                  (semantic-tag-get-attribute x :arguments))
                 ") {"
                 (if member-type
                     (concat
                      " return default("
                      member-type
                      ");")
                   "")
                 "} "))


               ((eq member-flavor 'variable) ;; it's an instance variable

                (concat
                 (mapconcat 'identity member-modifiers " ")
                 " "
                 member-type
                 " "
                 member-name
                 " = default("
                 member-type
                 "); "))

               (t
                "")))

        (setq synthetic-code (concat synthetic-code one-frag)))
      (setq member-list (cdr member-list)))
    synthetic-code))


(defun cscomp-consolidate-whitespace (s)
  "Collapse consecutive whitespace characters in the given string S
to a single space.
"
  ;; trim leading spaces
  (if (string-match "^\\([ \t\n\r\f\v]+\\)" s)
      (setq s (substring s (match-end 1))))

  ;; collapse multiple whitespace into one
  (while (string-match "\\([ \t\n\r\f\v]\\{2,\\}\\)" s)
    (setq s
          (concat
           (substring s 0 (match-beginning 1))
           " "
           (substring s (match-end 1)))))
  s)


(defun cscomp-escape-single-quotes (s)
  "Escape single-quotes in the given string S.  This is for use within
powershell.
"
  ;; escape single-quotes
  (while (string-match "\\(.+\\)'\\(.+\\)" s)
    (setq s
          (concat
           (substring s 0 (match-beginning 1))
           "`'"
           (substring s (match-end 1)))))
  s)


;;(setq cscomp-log-level 2)


(defun cscomp-get-var-type-given-decl (var-declaration
                                              var-index
                                              classname
                                              arglist
                                              instance-members)
  "Determines the type of the var declared in the given declaration.
VAR-DECLARATION is  the C# code that declares all the local vars up to
and including the local var of interest.

VAR-INDEX is the zero-based index of the local arg in that list,
that is of interest.  The initialization of that local var may
depend on the prior local vars, which is why we need the entire
var declaration list.

CLASSNAME is the name of the class in which the local vars
appear. This is used in the generated (synthetic) code, in case
there is a reference to a class-static member.

ARGLIST is a string with the arglist for the method that contains the
local variable in question.  This will satisfy dependencies on
local arguments in the initializer for the var, if any.

INSTANCE-MEMBERS is a C# code fragment defining instance members for the
synthetic class.  This will satisfy dependencies on instance members in
the initializer for the var, if any.

"
  (let* ((massaged-decl
          (cscomp-escape-single-quotes
           (cscomp-consolidate-whitespace var-declaration)))
         (namespaces (mapconcat 'identity (cscomp-referenced-assemblies-list) ","))
         (command-string
          (concat "[Ionic.Cscomp.Utilities]::GetTypeGivenVarDecl('"
                  massaged-decl
                  "','"
                  namespaces
                  "','" ;; for now, no additional assembly references
                  "',"
                  (number-to-string var-index)
                  ",'"
                  classname
                  "','"
                  arglist
                  "','"
                  (cscomp-consolidate-whitespace instance-members)
                  "')" )))
    (cscomp-send-string-to-shell command-string)))




(defun cscomp-get-completions-for-namespace (ns)
  "Gets a list of known completions (types and child namespaces)
for the given namespace. You must have loaded an assembly containing
types from the namespace, into the shell, for it to be known.
See `cscomp-load-assembly'."
  (interactive "sNamespace: ")
  (cscomp-invoke-shell-fn "GetCompletionsForNamespace" ns))



(defun cscomp-qualify-name (name)
  "determines if the thing is a type, namespace, or ...?

Result is

    (list \"type\"  name)

      or

    (list \"namespace\"  name)

      or

    (list \"unknown\"  name)

"

  (interactive "sname to qualify: ")
  (cscomp-invoke-shell-fn "QualifyName" name))


(defun cscomp-get-matches (fragment)
  "returns a list of all possible matches on a given partial name.
The return value is like this:

   (list (list \"type\"  \"fullNameOfType\")
         (list \"namespace\"  \"FullNameOfNamespace\"))

"
  (interactive "sfragment to match on: ")

  (let ((namespaces (mapconcat 'identity (cscomp-referenced-assemblies-list) ",")))

    (cscomp-send-string-to-shell
     (concat "[Ionic.Cscomp.Utilities]::GetMatches('"
             fragment
             "','"
             namespaces
             "')"))))



(defun cscomp-load-additional-assemblies (lib-list)
"Loads a set of assemblies into the csharp-shell, which then allows Cscomp
to do completion (etc) on the types in those libraries."
  (mapcar 'cscomp-load-assembly lib-list))


(defun cscomp-load-assembly (lib)
  "Loads a assembly into the csharp-shell, which then allows Cscomp
to do completion (etc) on the types in that library."
  (interactive "sLibrary: ")
  (cscomp-invoke-shell-fn "LoadOneAssembly" lib))



(defun cscomp-get-qualified-name (name)
  "Guess the fully qualified name of the class NAME, using the
list of referenced assemblies. It returns a string if the fqn
was found, or null otherwise."
  (interactive "sType name: ")
  (cscomp-log 1 "get-qualified-name (%s)" name)
  (let ((result (cscomp-type-exists name)))
    (if result result
      ;; else
      (let ((usinglist (cscomp-referenced-assemblies-list))
            fullname namespace )

        ;; usinglist is like this:
        ;; ("System"  "System.Collections"  "System.Collections.Generic"  "System.Reflection")

        (setq result nil)
        (while usinglist
          (setq namespace (car usinglist))
          (cscomp-load-assembly namespace)
          (setq fullname (concat namespace "." name))
          (cscomp-log 2 "checking this type: '%s'" fullname)
          (if (cscomp-type-exists fullname)
              (setq result fullname
                    usinglist nil)
            (setq usinglist (cdr usinglist))))

        (cscomp-log 1 "get-qualified-name rtns: '%s'" result)

        result))))



;; (defun cscomp-flush-typeinfo-cache ()
;;   "Flushes all entries in the completion cache"
;;   (interactive)
;;   (setq cscomp-typeinfo-cache nil))
;;
;;
;; (defun cscomp-flush-classes-in-cache (class-list)
;;   "Flushes all the classes in CLASS-LIST as entries of cache."
;;   (let ((temp (nth 0 cscomp-typeinfo-cache))
;;         (index -1)
;;         (found nil)
;;         (class (car class-list)))
;;     (while class
;;       (while (and temp (not found))
;;         (setq index (1+ index))
;;         (setq temp (nth index cscomp-typeinfo-cache))
;;         (if (string= (car temp) class)
;;             (setq found t)))
;;       (if found
;;           (setq cscomp-typeinfo-cache
;;                 (nthcdr (1+ index) cscomp-typeinfo-cache)))
;;       (setq class-list (cdr class-list))
;;       (setq class (car class-list))
;;       (setq found nil))))


(defun cscomp-add-to-typeinfo-cache (name typeinfo)
  (let (new-entry new-list)
    (if (nth cscomp-typeinfo-cache-size cscomp-typeinfo-cache)
        (progn
          (setq new-entry (list name typeinfo))
          (setq new-list (list new-entry nil))
          (setcdr new-list (cdr cscomp-typeinfo-cache))
          (setq cscomp-typeinfo-cache new-list)
          (cscomp-log 1 "cache is full")   )
      ;;else
      (setq cscomp-typeinfo-cache
            (append
             cscomp-typeinfo-cache
             (list (list name typeinfo)))))))


(defun cscomp-get-typeinfo-from-cache (name)
  (let ((temp (nth 0 cscomp-typeinfo-cache)) (index -1) (found nil))
    (while (and temp (not found))
      (setq index (1+ index))
      (cscomp-log 2 "looking at cache item %d" index)
      (setq temp (nth index cscomp-typeinfo-cache))
      (if (string= (car temp) name)
          (setq found t)))
    (if found
        (progn
          (cscomp-log 3 "cscomp-get-typeinfo-from-cache: HIT name(%s) r(%s)"
                    name (prin1-to-string (nth 1 temp)))
          (nth 1 temp))
      (cscomp-log 1 "cscomp-get-typeinfo-from-cache: MISS name(%s)" name)
      nil)))


(defun cscomp-get-typeinfo (name)
  "Return the class info list for the class NAME. This function first
checks to see if the class info is cached. If so, it returns the
cached class info. Otherwise, it creates the type info list. Each
element of the list returned by this function is itself a list whose
car is a possible completion and whose cdr gives additional
informations on the completion - the property type, or the param
list and return type for a method, etc."
  (interactive "sTypename: ")

  (cscomp-log 2 "cscomp-get-typeinfo name(%s)...trying cache..." name)

  (let ((type-info (cscomp-get-typeinfo-from-cache name))
        (usinglist (cscomp-referenced-assemblies-list))
        namespace
        qualified-type )

    ;; load all the assemblies mentioned in the using clauses
    (while usinglist
      (setq namespace (car usinglist))
      (cscomp-load-assembly namespace)
      (setq usinglist (cdr usinglist)))

    (if (null type-info)
        (progn
          (setq qualified-type
                (csharp-shell-exec-and-eval-result (concat
                                    "[Ionic.Cscomp.Utilities]::QualifyType('"
                                    ;;(cscomp-escape-string-for-powershell name)
                                    ;; dont need to escape if using single quotes
                                     name
                                    "')")))

          (cscomp-log 1 "cscomp-get-typeinfo...(%s)..." (prin1-to-string qualified-type))

          (if qualified-type
              (setq type-info
                    (csharp-shell-exec-and-eval-result (concat "[Ionic.Cscomp.Utilities]::GetTypeInfo('"
                                                (car qualified-type) ; type name
                                               "', '"
                                               (cadr qualified-type) ; assembly name
                                               "')" ))))
          (if type-info
              (cscomp-add-to-typeinfo-cache name type-info))))

    type-info))


(defun cscomp-get-type-ctors (type-name)
  "Retrieve constructors from CscompShell for the type named by TYPE-NAME.
"
  (interactive "sType name: ")
  (cscomp-invoke-shell-fn "GetConstructors" type-name))




(defun cscomp-split-by-dots (s)
  "When the string contains a dot, this fn returns a 2-element
list (TOKEN1 TOKEN2).  TOKEN1 is the substring of s that precedes
the last dot.  TOKEN2 is the substring that follows the last dot.

When the string does not contain a dot, this fn returns a
2-element list in which the first element is nil and the 2nd
element is the entire string.

"
  (cscomp-log 2 "cscomp-split-by-dots: s[%s]" s)
  (if (string-match "\\(.*\\)\\.\\(.*\\)" s)
      (let ((result (list (match-string 1 s) (match-string 2 s))))
        (cscomp-log 2 "cscomp-split-by-dots: %s" (prin1-to-string result))
        result)
    (list nil s))) ;; it's a single atom




(defun cscomp-parse-csharp-expression-before-point ()
  "Parses the text at point, and returns the results.

The retval is a list (POSN (TOKEN1 TOKEN2)) , where POSN is the position
in the buffer of the beginning of TOKEN1 and TOKEN1 and TOKEN2
are the two tokens surrounding the prior dot.

For example, suppose System.Diagnostics.D were the name at
point. This function would return the list

    (888 (\"System.Diagnostics\" \"D\"))

...if 888 was the position of the beginning of the word System.

It's just an exercise in syntactically-aware string parsing.

If the text preceding point doesn't look like two tokens, this fn
returns nil.

"
  (cscomp-log 2 "parse-csharp-expression-before-point: point(%d)" (point))

  (interactive)
  (save-excursion
    (let ((opoint (point))
          (cycle-count 0)
          (dot-count 0)
          m1
          (regex "[-\\+\\*\\/%,;( {})=\\.]")
          snip done
          (paren-depth 0)
          ;;           (skip-back '(lambda ()
          ;;                         (skip-chars-backward "\t ")
          ;;                         (backward-char)))
          )


      (while (not done)

        (skip-chars-backward "\t ")

        (cond

         ((and (eq cycle-count 0) ;; first time through
               (eq (char-before) 40)) ;; 40 = open paren - means we want fn completion
          (backward-char))             ;; backup

         ((eq (char-before) 34) ;; 34 = doublequote
          (backward-char 1)     ;; move back into the quoted string
          (let ((limits (c-literal-limits)))
            (if (consp limits)
                (c-safe (goto-char (car limits)))))) ;; goto beginning of literal string

         ;;          ((eq (char-before) 41)       ;; 41 = close paren - means we want fn completion
         ;;           (incf paren-depth)
         ;;           (backward-char))
         ;;
         ;;          ((and
         ;;            (eq (char-before) 40)      ;; 40 = open paren
         ;;            (> paren-depth 0))         ;; we have a pending close paren (moving backwards)
         ;;           (decf paren-depth)
         ;;           ;;(funcall skip-back))
         ;;           (backward-char))

         ;;          ((or
         ;;            (eq (char-before) 32)       ;; space
         ;;            (eq (char-before) ?\t))     ;; tab
         ;;          (funcall skip-back))

         ((re-search-backward regex nil t)
          (cond

           ((eq (char-after) 41) ;; 41 = close paren - means we want fn completion
            (incf paren-depth)
            t)

           ((and
             (eq (char-after) 40) ;; 40 = open paren
             (> paren-depth 0)) ;; we have a pending close paren (moving backwards)
            (decf paren-depth)
            ;;(funcall skip-back))
            t)


           ((or
             (eq (char-after) 32)   ;; space
             (eq (char-after) ?\t)) ;; tab
            t)                      ;; do nothing

           ((eq (char-after) ?.)
            (setq m1 (point)
                  regex "[-\\+\\*\\/%,;( {})=]"))

           (t
            (setq done t))))

         (t
          (backward-char)))

        (incf cycle-count)) ;; count of steps backward

      (setq snip
            (cscomp-consolidate-whitespace
             (buffer-substring-no-properties (1+ (point)) opoint)))

      (cscomp-log 2 "parse-expression-before-point: B snip(%s)" snip)
      (list (1+ (point)) (cscomp-split-by-dots snip)))
    ))




(defun cscomp-qualify-local-var (symbol opoint)
  "Use the semantic lex/analysis results to classify the
name as a local variable. Returns a list, 1st elt is the
variable type, and the second elt is a vector, containing
the position of its definition.

See also `cscomp-qualify-instance-var'.
"
  (cscomp-start-stripped-semantic)
  ;;(semantic-lex (point-min) (point-max) 100)
  (let ((locals (semantic-get-local-variables))
        (args (semantic-get-local-arguments))
        (result nil)
        (decl-count 0)
        (prior-var-decls ""))

    (while (and locals (not result))
      (let* ((x (car locals))
             (var-name (car x))
             (var-type (cadr (nth 2 x)))
             (var-pos (nth 4 x)))  ;; pair: start and end of var decl

        ;; The simple string= test will give a false positive if there's
        ;; a foreach loop variable in a prior
        ;; foreach loop with the same name as the one the user
        ;; wants completion on.
        ;;
        ;; This is only an issue with two or more foreach loops, each of
        ;; which create a separate naming scope, and which have the same
        ;; variable name.  All those foreach variables will be reported
        ;; as "local variables" by semantic. But, not all of them are in
        ;; scope for the completion we're performing right now.
        ;;
        ;; This needs to do something more intelligent with
        ;; the declaration. If I had a way to interrogate the scope of
        ;; the variable decl, that would help. But right now I don't have
        ;; that.

        ;; I think I need a better understanding of the semantic.el
        ;; package.
        ;; =======================================================


        ;; if this decl ends *before* the point at which the user is
        ;; asking for completion.
        (if (<  (elt var-pos 1) opoint)

            ;; If this var decl is the same name as the one
            ;; we want.
            (if (string= var-name symbol)
                (progn

                  ;; Handle var types - need to determine the actual type.
                  ;;
                  ;; To do that, compile the var declaration, then inspect the IL
                  ;; to determine the var types.
                  ;;
                  ;; This engine will determine the var type, in some portion% of
                  ;; the cases.
                  ;;
                  ;; It will handle:
                  ;;
                  ;;  - simple var declarations that have no dependencies on other vars
                  ;;  - cascaded var decls that depend on other local vars.
                  ;;
                  ;; For now, it will not handle:
                  ;;
                  ;;  - var that depends on a method argument
                  ;;  - var whose initialization depends on an instance var
                  ;;  - var decls in foreach loops
                  ;;

                  ;; if the type of the variable is "var"
                  (if (string= var-type "var")
                      (let* ((this-decl
                              (buffer-substring-no-properties (elt var-pos 0) (elt var-pos 1)))

                             (containing-type (cscomp-get-current-class))
                             (member-list (cscomp-get-members-of-class containing-type))
                             (name-of-containing-type  (car containing-type))

                             (inferred-type
                              (cscomp-get-var-type-given-decl
                               (concat prior-var-decls " " this-decl)
                               decl-count
                               name-of-containing-type
                               (cscomp-produce-csharp-arglist-block-from-dbrecord args)
                               (cscomp-produce-instance-member-code-fragment member-list)
                               )))

                        (if (and inferred-type (string= (car inferred-type) "type"))
                            (setq var-type (cadr inferred-type))
                          (message "%s" (prin1-to-string inferred-type))
                          )))

                  (cscomp-log 2 "cscomp-qualify-local-var: found %s (%s)"
                            symbol var-type)
                  (setq result (list var-type var-pos)))

              ;; else - remember it. We may need it later
              (let* ((this-var-decl
                      (buffer-substring-no-properties (elt var-pos 0) (elt var-pos 1)))
                     (tokens (split-string this-var-decl "[ \t]" t)))

                ;; include decl in prior decls if not "foreach(var foo in X)"
                (if (or (< (length tokens) 4)
                        (not (string= (nth 3 tokens) "in")))
                    (setq prior-var-decls (concat prior-var-decls " " this-var-decl)
                          decl-count (1+ decl-count))

                  ;; else - it's a foreach loop

                  ;; If performing completion on a var within the loop that
                  ;; depends on the loop variable, then we need to infer
                  ;; the type of the foreach loop variable here.
                  ;;
                  ;; But, I'm punting.

                  )))))
      (setq locals (cdr locals)))

    result))





(defun cscomp-qualify-instance-var (symbol)
  "Use the semantic lex/analysis results to classify the
name as an instance variable. Returns a list, 1st elt is the
variable type, and the second elt is a vector, containing
the position of its definition.

See also `cscomp-qualify-local-var'.

"
  (cscomp-start-stripped-semantic)
  ;;(semantic-lex (point-min) (point-max) 100)
  (let ((ivars (cscomp-instance-vars))
        (result nil))
    (while (and ivars (not result))
      (let* ((x (car ivars))
             (var-name (car x))
             (var-type (cadr (nth 2 x)))
             (var-pos (nth 4 x)))
        (if (string= var-name symbol)
            (progn
              (cscomp-log 2 "cscomp-qualify-instance-var: found %s (%s)"
                       symbol var-type)
            (setq result (list var-type var-pos))))
      (setq ivars (cdr ivars))))
    result))




(defun cscomp-start-stripped-semantic ()
  "Enable a stripped-down semantic for usage with csharp-completion.
Semantic is very ambitious and tries to do many things, including
predictive completion, modified code formatting, and other
things.  We don't want all that for this simple completion
module. So this method starts a stripped-down version of
semantic.

"
  (interactive)

  (if (null semantic-load-system-cache-loaded)
      (progn
        ;;(semantic-lex (point-min) (point-max) 100)
        (semantic-fetch-tags)
        (global-semantic-idle-scheduler-mode 1)
        ;;(global-semanticdb-minor-mode 1)
        ;; This loads any created system databases which get linked into
        ;; any searches performed.
        (setq semantic-load-system-cache-loaded t)
        )))





;;     (let (start
;;           varname
;;           (curcar (char-before))
;;           found
;;           (original-point (point))
;;           intermediate-point
;;           beg-point
;;           first-part
;;           second-part
;;           (bracket-count 0)
;;           (paren-count 0))
;;
;;
;;       (while (null found)
;;         (cond
;;
;;          ;; car is a-z, A-Z 0-9 or greater than 127, or slash or underscore
;;          ((or (and (>= curcar ?a) (<= curcar ?z))
;;               (and (>= curcar ?A) (<= curcar ?Z))
;;               (and (>= curcar ?0) (<= curcar ?9))
;;               (>= curcar 127)
;;               (member curcar '(?_ ?\\ )))
;;           ;; back up!
;;           (forward-char -1))
;;
;;          ;; curchar is a dot
;;          ((eq ?. curcar)
;;           (setq found (point)))
;;
;;          ;; else
;;          (t
;;           (setq found t)))
;;
;;
;;         (setq curcar (char-before)))
;;
;;       ;; we've backed-up to...the nearest dot or non- alphanumeric char.
;;
;;
;;       ;; ??
;;       (setq intermediate-point (point))
;;
;;
;;       (if (not (eq t found))  ;; not t means we found a dot
;;           (progn
;;
;;             ;; get the char before
;;             (setq curcar (char-before))
;;             (while (or (and (>= curcar ?a) (<= curcar ?z))
;;                        (and (>= curcar ?A) (<= curcar ?Z))
;;                        (and (>= curcar ?0) (<= curcar ?9))
;;                        (>= curcar 127)
;;                        (and (eq curcar ? ) (or (< 0 paren-count) (< 0 bracket-count)))
;;                        (member curcar '(?\. ?\_ ?\\ ?\( ?\) ?\, ?\[ ?\])))
;;               (cond
;;                ((eq curcar ?\) )
;;                 (setq paren-count (1+ paren-count)))
;;                ((eq curcar ?\( )
;;                 (setq paren-count (1- paren-count)))
;;                ((eq curcar ?\] )
;;                 (setq paren-count (1+ bracket-count)))
;;                ((eq curcar ?\[ )
;;                 (setq paren-count (1- bracket-count))))
;;               (forward-char -1)
;;
;;               (setq curcar (char-before)))
;;
;;
;;             (setq beg-point (point))
;;
;;             (set-marker cscomp-current-beginning intermediate-point)
;;
;;             (set-marker cscomp-current-end original-point)
;;
;;             (setq first-part (buffer-substring-no-properties beg-point (- intermediate-point 1)))
;;
;;             (setq first-part (cscomp-isolate-to-complete first-part))
;;
;;             (string-match " *\\(.*\\)" first-part)
;;
;;             (setq first-part (substring first-part (match-beginning 1) (match-end 1)))
;;
;;             (setq second-part (buffer-substring-no-properties intermediate-point original-point))
;;
;;             (list first-part second-part))
;;
;;         nil))))



(defun cscomp-build-clist-for-ns (nsinfo)
  "Build a completion list from the NSINFO list, as returned by the
Ionic.Cscomp.Utilities.GetCompletionsForNamespace function.

If the incoming NSINFO list looks like this:

  (\"System.Collections.Generic\"
     (types (\"ArraySortHelper`1\"
             \"ArraySortHelper`2\"
             \"ByteEqualityComparer\"
             \"Comparer`1\"
             \"Dictionary`2\"
             \"EqualityComparer`1\"))
     (namespaces (\"Something1\"
             \"Something2`2\")))

Then the return value is like this:

  (\"ArraySortHelper<T1,T2> | (type)\"
   \"ArraySortHelper<T1> | (type)\"
   \"ByteEqualityComparer | (type)\"
   \"Comparer<T1> | (type)\"
   \"Dictionary<T1,T2> | (type)\"
   \"EqualityComparer<T1> | (type)\"
   \"Something1 | (type)\"
   \"Something2`2 | (type)\")

"
  (let* ((typelist (cadr (cadr nsinfo)))
         (nslist
          (mapcar '(lambda (item) (concat item " | (namespace)") )
                  (cadr (caddr nsinfo)))))
    (cscomp-fix-generic-method-strings typelist)
    (setq typelist (mapcar '(lambda (item) (concat item " | (type)") )
                           typelist))
    ;;(reverse (sort (append typelist nslist) 'string-lessp))
    (sort (append typelist nslist) 'string-lessp)
  ))



(defun string-replace-char (s c1 c2)
  "Replace all occurrences of char C1 in string with char C2.
Return the modified string."
  (let ((string-len  (length s))
        (ix 0))
    (while (< ix string-len)
      (if (eq (aref s ix) c1)
          (aset s ix c2))
      (incf ix)))
  s)



(defun cscomp-fix-one-generic-method-string (descrip)
  "Reformat the generic type string like System.Action`1
into a C#-friendly System.Action<T1> . Returns the reformatted
string.  Returns nil if reformat was unnecessary.
"
  (cond

   ;; input:  System.Converter`2[T,TOutput]
   ;; output: System.Converter<T,TOutput>
   ((string-match "^\\(.+\\)`[1-9]\\[\\([^]]+\\)\\]$" descrip)
    (concat
     (match-string 1 descrip)
     "<"
     (match-string 2 descrip)
     ">"))

   ;; input:  System.Collections.Generic.Dictionary`2
   ;; output: System.Collections.Generic.Dictionary<T1,T2>
   ((string-match "^\\(.+\\)`\\([1-9]\\)\\(.*\\)$" descrip)
    (let ((z (string-to-number (match-string 2 descrip)))
          (new-name (concat (match-string 1 descrip) "<"))
          (i 0))
      (while (< i z)
        (setq i (1+ i)
              new-name (concat new-name (format "T%d" i)))
        (if (< i z)
            (setq new-name (concat new-name ","))))
      (setq new-name (concat new-name ">" (match-string 3 descrip)))))

   ;; anything else
   (t nil)))



(defun cscomp-fix-generic-method-strings (clist)
  "Fixup a list of strings, that may have generic method types
in them, to have C#-friendly formats.  This fn does the modifications
in place. The return value is the original list, with the internal values
modified.

"
  (let ((count  (list-length clist))
        (n 0)
        new-name
        cur-elt)
    (while (< n count)
      (setq cur-elt (nth n clist))
      (if (setq new-name (cscomp-fix-one-generic-method-string cur-elt))
          (setf (nth n clist) new-name))
      ;; the following will affect the string in the list
      (string-replace-char cur-elt ?+ ?.)
      (setq n (1+ n))))
  clist)



(defun cscomp-fix-generics-in-descrip-line (descrip-line)
  "Fixup a string, a description of a method or property, to
have C#-friendly generic types.

Eg, convert  (Method) public (System.Converter`2[T,TOutput] converter) returns List`1[TOutput]

to  (Method) public (System.Converter<T,TOutput> converter) returns List<TOutput>

"
  (let ((tokens (split-string descrip-line "[ \t]" t)))
    (cscomp-fix-generic-method-strings tokens)
    (mapconcat 'identity tokens " ")))





(defun cscomp-build-clist-for-type (typeinfo)
  "Build a completion list from the TYPEINFO list, as returned by the
Ionic.Cscomp.Utilities.GetTypeinfo function in the CscompShell. The list
is used when performing completions on an instance of a given type.

For input that looks like this:

  (\"System.IO.DriveInfo\" 'type
   ((\"Name\"               'property \"System.String\" (typemodifiers \"readonly\" \"public\"))
    (\"AvailableFreeSpace\" 'property \"System.Int64\"  (typemodifiers \"readonly\" \"public\"))
    (\"TotalFreeSpace\"     'property \"System.Int64\"  (typemodifiers \"readonly\" \"public\"))
    (\"VolumeLabel\"        'property \"System.String\" (typemodifiers \"public\")))

   ((\"Equals\" method \"System.Boolean\" (\"System.Object obj\") (typemodifiers \"public\"))
    (\"GetDrives\" method \"System.IO.DriveInfo[]\" nil (typemodifiers \"public\" \"static\"))
    (\"ToString\" method \"System.String\" nil (typemodifiers \"public\"))))


The output looks like this:

  ((\"ToString\"           \"(Method) public () returns System.String\")
   (\"GetDrives\"          \"(Method) public static () returns System.IO.DriveInfo[]\")
   (\"Equals\"             \"(Method) public (System.Object obj)  returns System.Boolean\")
   (\"VolumeLabel\"        \"(Property) public System.String\")
   (\"TotalFreeSpace\"     \"(Property) readonly public System.Int64\")
   (\"AvailableFreeSpace\" \"(Property) readonly public System.Int64\")
   (\"Name\"               \"(Property) readonly public System.String\"))

"

  (let (result
        (tname   (car typeinfo))
        (props   (caddr typeinfo))
        (methods (cadddr typeinfo))
        (fields  (caddr (cddr typeinfo)))
        modifiers)

    (cscomp-log 2 "cscomp-build-clist-for-type: typename(%s)" tname)

    (while props
      (let ((one-prop (car props) ) )
        (setq modifiers
              (mapconcat 'identity (cdr (nth 3 one-prop)) " "))
        (setq result
              (append (list
                       (list
                        (car one-prop)   ;; the name of the property

                        ;; additional information about the property
                        (concat "(Property) "
                                modifiers  ;; modifiers on this prop
                                " "
                                (nth 2 one-prop)  ;; type of the prop
                                )))
                      result)))
      (setq props (cdr props)))

    (while methods
      (let ((one-method (car methods)) params)
        (setq modifiers
              (mapconcat 'identity (cdr (nth 4 one-method)) " "))
        (setq params
              (if (nth 3 one-method)
                  (concat "("
                          (mapconcat 'identity (nth 3 one-method)  ", ")
                          ")")
                ;; else
                "()" ))

        (setq result
              (append (list
                       (list
                        (car one-method)   ;; the name of the method

                        ;; additional information about the method (in a string)
                        (concat "(Method) "
                                modifiers           ;; modifiers on this prop
                                "  "
                                params
                                "  returns "
                                (nth 2 one-method)  ;; return type of the method
                                )
                        ))
                      result)))

      (setq methods (cdr methods)))


    (while fields
      (let ((one-field (car fields)))
        (setq modifiers
              (mapconcat 'identity (cdr (nth 3 one-field)) " "))
        (setq result
              (append (list
                       (list
                        (car one-field)   ;; the name of the field

                        ;; additional information about the field (in a string)
                        (concat "(Field) "
                                modifiers           ;; modifiers on this prop
                                "  "
                                (nth 2 one-field)  ;; type of the field
                                )
                        ))
                      result)))

      (setq fields (cdr fields)))


    (cscomp-log 3 "cscomp-build-clist-for-type: result: "
              (prin1-to-string result))
    ;;(print result (get-buffer "*Messages*"))
    result))



(defun cscomp-build-clist-for-ctors (ctor-info)
  "Build a completion list from the CTOR-INFO list, as returned by the
Ionic.Cscomp.Utilities.GetConstructors function in the CscompShell. The list
is used when performing completions on a constructor for a given type.

The input looks like:
  (\"System.String\" 'type
               (:constructors

                ((:typemodifiers (\"public\")
                  :arguments ((\"value\" 'variable
                          (:type \"System.Char*\"))))

                 (:typemodifiers (\"public\")
                  :arguments ((\"value\" 'variable
                                     (:type \"System.Char*\"))
                   (\"startIndex\" 'variable
                               (:type \"System.Int32\"))
                   (\"length\" 'variable
                           (:type \"System.Int32\"))))

The output is a completion list, which is a list of (NAME DESCRIP) pairs.
The DESCRIP of each pair should have the arguments for each constructor.

That list is later transformed into a structure that is suitable
for use in a popup menu.

"
  (let (result
        (tname   (car ctor-info))
        (ctors   (cadr (caddr ctor-info)))
        ;;(methods (cadddr typeinfo))
        )

    (cscomp-log 2 "cscomp-build-clist-for-ctor: typename(%s)" tname)

    (while ctors
      (let* ((one-ctor (car ctors))
             (modifiers (mapconcat 'identity  (nth 1 one-ctor) " "))
             (params
              (if (nth 3 one-ctor)
                  (concat "("
                          (mapconcat
                           '(lambda (x)
                              (concat
                               (cadr (caddr x))
                               " "
                               (car x)))
                           (nth 3 one-ctor)  ", ")
                          ")")
                ;; else
                "()" )))
        (setq result
              (append (list
                       (list
                        tname  ;; name of the ctor

                        ;; description for this ctor
                        (concat "(Constructor) "
                                modifiers  ;; modifiers on this prop
                                " "
                                params
                                )))
                      result)))
      (setq ctors (cdr ctors)))

    (cscomp-log 3 "cscomp-build-clist-for-ctors: result: "
              (prin1-to-string result))
    result))



;; (defun cscomp-build-information-for-completion (lst)
;;   (let ((result (concat (car (cdr lst)) " " (car lst) "(")))
;;     (setq lst (cdr (cdr lst)))
;;     (while lst
;;       (setq result (concat result (car lst)))
;;       (setq lst (cdr lst))
;;       (if lst
;;           (setq result (concat result ", "))))
;;     (setq result (concat result ")"))
;;     result))




;; (defun cscomp-popup-xemacs-completion-menu (completion-list)
;;   (let* ((items
;;        (sort
;;         ;; Change each item in the completion list from the form
;;         ;;   return-value method-name(args)
;;         ;; to the form
;;         ;;   method-name(args) : return-value
;;         (mapcar
;;          (lambda (completion)
;;            (let ((completion-short (nth 0 completion))
;;                  (completion-long (nth 1 completion)))
;;              (if completion-long
;;                  (let ((chop-pos (string-match " " completion-long)))
;;                    (concat
;;                     (substring completion-long (1+ chop-pos)
;;                                (length completion-long))
;;                     " : "
;;                     (substring completion-long 0 chop-pos)))
;;                completion-short)))
;;          completion-list)
;;         'string<))
;;       (menu
;;        (cons
;;         "Completions"
;;         (mapcar
;;          (lambda (item)
;;            (vector item (list 'cscomp-insert-completion item)))
;;          items))))
;;     (popup-menu-and-execute-in-window menu (selected-window))))




(defun cscomp-sort-completion-list (lst)
  (if (consp (car lst))
      (sort lst
            '(lambda (e1 e2)
               (string< (car e1) (car e2))))
    (sort lst
          '(lambda (e1 e2)
             (string< e1 e2)))))


;; (defun cscomp-sort-completion-list (lst)
;;  ;;  "sort LST in place."
;;   (sort lst
;;         '(lambda (e1 e2)
;;            (if (consp e1) ;; is the list elt a consp?
;;                ;; yes, we're completing on a type. Sort on the car of that list
;;                (string< (car e1) (car e2))
;;              ;; no, we're completing on a namespace or local var.
;;              ;; The element should be a string.  Sort on it directly.
;;              (string< e1 e2)
;;              ))))



(defun cscomp-find-all-ns-completions (fragment lst &optional exact-match )
  "Find all completions for FRAGMENT in LST.  If EXACT-MATCH is true,
then...?  LST is a simple list of strings, as obtained from
`cscomp-build-clist-for-ns'.
"
  (let ((result nil))
    (while lst
      (let ((candidate (car lst)))
        (if (or (and exact-match (string= fragment candidate))
                (and (not exact-match) (equal 0 (string-match (concat "^" fragment) candidate))))
            (progn
              (message "  HIT")
              (setq result (append (list candidate) result)))))
      (setq lst (cdr lst)))

    (setq cscomp-current-fragment fragment)

    ;; sort, and then transform into a list of (NAME DESCRIP) pairs:
    (mapcar
     '(lambda (item)
        (if (string-match "^\\([^|]+\\) +| +\\(.+\\)$" item)
            (list (match-string 1 item)
                  (match-string 2 item))
          item))
     (cscomp-sort-completion-list result))))




(defun cscomp-find-all-type-completions (fragment lst static &optional exact-match)
  "Find all completions in LST for FRAGMENT.  In practice,
FRAGMENT is a string containing the characters following the last
dot.  Eg, if the user typed System.IO.Dir, then FRAGMENT would be
\"Dir\".

LST is a list obtained from `cscomp-build-clist-for-type'.

If STATIC is non-nil, then match only non-static props/fields/methods.
Otherwise, match only static fields/props/methods.

If EXACT-MATCH is true, then only the completions that match the
FRAGMENT exactly are returned.  Normally, EXACT-MATCH is not
true, and in thi case, the completions that start with the
FRAGMENT are returned.

"

;; For illustration, the LST for DriveInfo looks like this:
;; (
;;  ("Name" "(Property) readonly public System.String")
;;  ("DriveType" "(Property) readonly public System.IO.DriveType")
;;  ("DriveFormat" "(Property) readonly public System.String")
;;  ("IsReady" "(Property) readonly public System.Boolean")
;;  ("AvailableFreeSpace" "(Property) readonly public System.Int64")
;;  ("TotalFreeSpace" "(Property) readonly public System.Int64")
;;  ("TotalSize" "(Property) readonly public System.Int64")
;;  ("RootDirectory" "(Property) readonly public System.IO.DirectoryInfo")
;;  ("VolumeLabel" "(Property) public System.String")
;;  ("Equals" "(Method) public  (System.Object)  returns System.Boolean")
;;  ("GetDrives" "(Method) public static  ()  returns System.IO.DriveInfo[]")
;;  ("GetHashCode" "(Method) public  ()  returns System.Int32")
;;  ("GetType" "(Method) public  ()  returns System.Type")
;;  ("ToString" "(Method) public  ()  returns System.String")
;;  )


  (let ((result nil))
    (while lst
      (let* ((candidate (car lst))
             (member-name (car candidate))
             (descrip (cadr candidate))
             (is-static (string-match " static " descrip))
             (is-match  (and
                         (if static is-static (not is-static))
                         (if exact-match
                             (string= fragment member-name)
                           (equal 0 (string-match fragment member-name))))))

        (cscomp-log 3 "cscomp-find-all-type-completions, looking at %s %s"
                  (prin1-to-string candidate)
                  (if is-match "MATCH" ""))

      (if is-match
            (setq result (append (list candidate) result))))

      (setq lst (cdr lst)))

    (cscomp-log 3 "cscomp-find-all-type-completions, result: %s"
                (prin1-to-string result))
      ;;(print result (get-buffer "*Messages*"))

    (setq cscomp-current-fragment fragment)
    (cscomp-sort-completion-list result)
    ))






(defvar cscomp-primitive-types
  (list
   '("string" "System.String")
   '("byte"   "System.Byte")
   '("sbyte"  "System.SByte")
   '("char"   "System.Char")
   '("double" "System.Double")
   '("float"  "System.Float")
   '("bool"   "System.Boolean")
   '("int"    "System.Int32")
   '("long"   "System.Int64")
   '("short"  "System.Int16")
   '("uint"    "System.UInt32")
   '("ulong"   "System.UInt64")
   '("ushort"  "System.UInt16")
   )

  "a list that maps primitive C# types to their unboxed types.")



(defun cscomp-map-primitive-type (type)
"Maps the type to the expanded name of the  type, and returns that
expanded name. For example, bool => System.Boolean
If type is not a primitive type, the result is nil."
  (if (member type (mapcar 'car cscomp-primitive-types))
    (let ((result nil)
          (lst cscomp-primitive-types)
          item )
    (while lst
      (setq item (car lst))
      (if (string= type (car item))
          (and
           (setq result (cadr item))
           (setq lst nil))
        ;else
        (setq lst (cdr lst))))
    result )
    nil ))





(defun cscomp-find-completion-for-split (split opoint beginning-of-token1)

  "SPLIT is a list of (TOKEN1 TOKEN2), as returned from
`cscomp-split-by-dots'.

OPOINT is the point at which the user has requested completion.

If the user has typed something followed by a dot, followed by an
optional fragment after the dot, then TOKEN1 may be a C#
namespace or class, or a variable name, or explicitly, \"this\" .
TOKEN2 may be the empty string, or a fragment of a name.  The
goal is to find a completion for these two things.

Example: if SPLIT is (\"System\" \"Diag\") then the returned
completion list will contain \"Diagnostics\".

If the completion being requested is on a type, OPOINT is used to
determine whether to present type names, or actual constructors.
Do the latter if the new keyword precedes the
thing-to-be-completed.

"

  (cscomp-log 2 "cscomp-find-completion-for-split: A: '%s' '%s'"
            (car split) (cadr split))

  ;;
  ;; p1 is what precedes the final dot, p2 is what follows.
  ;; When there is no dot, p1 is nil.
  ;;
  ;;
  ;; Possibilities:
  ;;
  ;; 1. p1 is a namespace like System.Xml  p2 is empty, or a fragment, like XPa.
  ;;    In this case we need to find types or child namespaces in the given
  ;;    namespace that match the given fragment.
  ;;
  ;; 2. p1 is "this" and p2 is a fragment of a name of a class field/prop/method.
  ;;    In this case, complete on the members of the enclosing class.
  ;;
  ;; 3. p1 is the name of a local variable, and p2 is empty or a fragment.
  ;;    In this case, complete with a field, prop, or method on that variable.
  ;;    Must know or learn the type of the variable in that case.
  ;;
  ;; 4. p1 is nil, and p2 is the partial name of a local variable.
  ;;    In this case, complete with the name of a local field, prop, or method
  ;;    that matches
  ;;
  ;; 5. p1 is the name of a type, like System.Console, and p2 is something.
  ;;    In this case, complete with static field, prop, and methods on
  ;;    that type.
  ;;
  ;; 6. p1 is the name of a primitive type, like byte or int.
  ;;    In this case, replace it with the expanded type, and complete as
  ;;    in case 5.
  ;;
  ;; 7. others?
  ;;
  ;; =============================================
  ;;
  ;; There are two sources for completion possibilities.  For known types,
  ;; the possibilities come from introspection, done through the CscompShell.
  ;; So, for System.Xml.XmlDocument, we send a message to the shell and get back
  ;; all the known fields/props/methods of that .NET type.
  ;;
  ;; For unknown types - currently the only "unknown" qualifier is "this." -
  ;; use the parse results from semantic to find fields/props/methods.
  ;;

  (let* ((p1 (car split))
         (p2 (cadr split))
         (is-primitive    (cscomp-map-primitive-type p1))
         (is-local-var    (cscomp-qualify-local-var p1 opoint))
         (is-instance-var (cscomp-qualify-instance-var p1))
         (is-func         (string-match "($" p2))  ;; open-paren
         (is-ctor         (and                     ;; constructor
                           (null p1)
                           (string-match "^\\([ \t\n\r\f\v]*new[ \t\n\r\f\v]+\\)\\(.+\\)$" p2)))
         p1-flavor
         p1-type
         is-static
         r)

    (if is-ctor
        ;; reset the beginning marker
        (progn
          (set-marker cscomp-current-beginning
                      (+ cscomp-current-beginning
                         (match-end 1)))

          ;;(string-match "^\\([ \t]*new[ \t]+\\)\\(.+\\)$" p2)
          ;; rely on most recent string-match context being for is-ctor
          (setq p2 (substring p2 (match-beginning 2)))))




    ;; figure out what we're completing on.

    (cond
     (is-primitive
      ;; A static method/prop/field on a byte, int, char, etc.
      (cscomp-log 3 "cscomp-find-completion-for-split: is-primitive")
      (setq p1-flavor "type"
            p1-type is-primitive
            is-static t)) ;; get static methods/props/fields on System.Byte, etc

     (is-local-var
      ;; method, property, or field on  a local variable.
      (cscomp-log 3 "cscomp-find-completion-for-split: is-local-var")
      (setq p1-flavor "type"
            p1-type (or (cscomp-map-primitive-type (car is-local-var))
                        (car is-local-var))))

     (is-instance-var
      ;; it's an instance variable.
      (cscomp-log 3 "cscomp-find-completion-for-split: is-instance-var")
      (setq p1-flavor "type"
            p1-type (or (cscomp-map-primitive-type (car is-instance-var))
                        (car is-instance-var))))

     ((string= "this" p1)
      ;; complete on instance field/prop/method
      (setq p1-flavor "this"))


     ((and (null p1) ;; no prefix.
           is-func) ;; open paren is the last char, eg  "new TimeSpan(?"
      (setq r (cscomp-qualify-name (substring p2 0 -1)))
      (cond ((listp r)
             (if (string= (car r) "type")
                 (setq p1-flavor "namespace"
                       p1-type (and
                                (string-match "^\\(.+\\)\\..+$" (cadr r))
                                (match-string 1 (cadr r)))
                       p1 p1-type)
               (setq p1-flavor "local")))
            (t
             (setq p1-flavor "local"))))


     (is-ctor
      (setq p1-flavor "mixed")
      (if (null p1)
          (setq p1 p2)))


;;     ((and (null p1)   ;; no prefix.
;;        (t
;;         ;; It's not a ctor or static method.
;;         ;; Maybe it's completion on a known typename or namespace.
;;         (setq p1-flavor "mixed"))))

     (t
      (setq r (cscomp-qualify-name p1))
      (cond ((listp r)
             (progn
               (setq p1-flavor (car r))

               (if (string= p1-flavor "unknown")
                 ;; assume p1 is an expression.  Try to infer it's type.
                   (let ((inferred-type
                         (cscomp-get-var-type-given-decl (concat "var x = " p1 ";")
                                                          0
                                                          "NoMatter"
                                                          ""
                                                          "")))
                         (if (string= (car inferred-type) "type")
                             (setq p1-type (cadr inferred-type)
                                   p1-flavor "type")))


               ;; name qualification was successful
               (setq p1-type p1
                     is-static t)))) ;; if it's a type, we want static f/m/p only

            (t
             (error "qualify-name returns invalid results.")))))



    (cscomp-log 2 "cscomp-find-completion-for-split: B: p1(%s,%s) p2(%s) flav(%s)"
              p1 (if p1 p1-type "--")  (prin1-to-string p2) p1-flavor)


    ;; now collect completion options depending on the "flavor" of p1:
    ;; type == completing on m/f/p for a type, possibly with a fragment.
    ;;         p1 holds the typename from the buffer, could be partially qualified.
    ;;         p1-type holds the fully qualified type name. p2 holds the fragment.
    ;; namespace == completing on a  namespace.  The result can be a child
    ;;         namespace or a type within a namespace.
    ;; this == completing on a m/f/p of the local instance
    ;; local == completing on a m/f/p of local variables or method args
    ;; mixed == could be either a typename or a namespace name. This is
    ;;         the case when p1 is partial, and there is no dot.


    (cond

     ((string= p1-flavor "type")
      (let* ((type-info (cscomp-get-typeinfo p1-type))
            (full-list (cscomp-build-clist-for-type type-info)))
      (cond

       (is-func ;; it's a specific function on a type. Get the list of overloads.
          (setq cscomp-current-list
                (cscomp-find-all-type-completions (substring p2 0 -1) full-list is-static))
          (cscomp-log 3 "cscomp-find-completion-for-split: [%s]"
                    (prin1-to-string cscomp-current-list)))
       (t ;; could be anything: method, prop, field. Build the list.
          (setq cscomp-current-list
                (cscomp-find-all-type-completions p2 full-list is-static))

          (cscomp-log 3 "cscomp-find-completion-for-split: [%s]"
                    (prin1-to-string cscomp-current-list))))))


     ((string= p1-flavor "namespace")
      (cscomp-log 2 "cscomp-find-completion-for-split, is-func(%s) bot(%d)"
                is-func beginning-of-token1)
      (cond
       (is-func ;; it's a function on a type - see if a constructor
        (cond
         ((save-excursion
            (goto-char beginning-of-token1)
            (re-search-backward "\\<new[ \t\n\f\v]+" nil t))
          ;; it's a constructor
          (let* ((type-name (concat p1 "." (substring p2 0 -1)))
                 (ctor-info (cscomp-get-type-ctors type-name))
                 (full-list (cscomp-build-clist-for-ctors ctor-info)))

            ;; present all constructors
            (setq cscomp-current-list full-list)))

         (t
          ;; not a constructor.  Must be some other function.
          (error "completion on static functions is not (yet?) supported."))))

       (t ;; complete on a type name, or child namespace
        (let* ((type-list (cscomp-get-completions-for-namespace p1))
               (full-list (cscomp-build-clist-for-ns type-list)))

          (setq cscomp-current-list
                (cscomp-find-all-ns-completions p2 full-list))
          (cscomp-log 3 "cscomp-find-completion-for-split: [%s]"
                    (prin1-to-string cscomp-current-list))))))


     ((string= p1-flavor "local")
      (let ((instance-vars (cscomp-matching-instance-vars p2))
            (locals (cscomp-matching-local-vars p2)))
        (setq cscomp-current-list
                  (nconc instance-vars locals))))

     ((string= p1-flavor "this")
        (setq cscomp-current-list
              (cscomp-matching-instance-members p2)))


     ((string= p1-flavor "mixed")
      (setq r (cscomp-get-matches p2))
      (if (listp r)
          (setq cscomp-current-list
                (mapcar '(lambda (listitem)
                           (list
                            ;; the thing to insert
                            (if (and p1 (string-match (concat "^" p1)
                                                      (cadr listitem)))
                                (cadr listitem)
                              (string-match (concat "\\.\\(" p2 ".*\\)") (cadr listitem))
                              (match-string 1 (cadr listitem)))

                            ;; the description that will be visible in the menu
                            (concat (cadr listitem) " | (" (car listitem) ")")
                            ))
                        r))
        (error "Cannot complete.")))

     (t
      (cscomp-log 1 "cscomp-find-completion-for-split, unknown flavor (%s)"
                p1-flavor)
      (error "Cannot complete.")))))




;; (defun cscomp-string-starts-with (s arg)
;;   "returns t if string S starts with ARG. Else nil."
;;   (eq 0 (string-match arg s)))



(defun cscomp-yasnippet-for-arglist (method-name arglist)
  "Produce a snippet that's usable with ya-snippet, for the method
selected from the completion popup menu. The arguments are strings:
METHOD-NAME is the name of the method, and ARGLIST is a description
of the argument list for the selected method.

The return value is a list, which, if selected from the menu, will be
eval'd. The first element in the list is the literal, yas/expand-snippet,
and subsequent elements in the return value list are simply arguments
to that function.

"
  (let ((template "(") (str-ix 1) (arg-ix 1)
        (next-char (char-after))
        need-delete)

    ;; Build the template for yasnippet. Each replaceable
    ;; param is identified with ${N:foo} where N is 1,2,3...
    ;; and foo is the placeholder shown in the buffer, which
    ;; is then "typed over" by the user.
    ;;
    ;; In this case, the placeholder contains the type and the
    ;; name of the argument, from the reflection results.
    ;;
    (while (string-match "\\([^ ]+\\) \\([^,)]+\\)\\([,)]\\)"
                         arglist str-ix)
      (setq template (concat template "${"
                             (number-to-string arg-ix)
                             ":"
                             ;; type of the arg
                             (substring arglist
                                        (match-beginning 1)
                                        (match-end 1))
                             " "
                             ;; name of the arg
                             (substring arglist
                                        (match-beginning 2)
                                        (match-end 2))
                             "}"

                             ;; the following comma or closing paren
                             (if (string= (substring arglist
                                        (match-beginning 3)
                                        (match-end 3))
                                          ")")
                                 ;; If a close-paren is in the buffer,
                                 ;; cscomp will delete it, before inserting
                                 ;; the snippet.
                                 (progn
                                   ;; to delete the existing close paren
                                   (setq need-delete (eq next-char 41))
                                   ")")
                               ", "))

            str-ix (match-end 0)
            arg-ix (1+ arg-ix)))

    (if (eq (length template) 1)
        (setq template "()"))

    ;; Upon selection of this item from the menu, emacs will
    ;; call yas/expand-snippet. If necessary, emacs will
    ;; delete a char just before doing so.
    (if need-delete
        (list 'progn
          (list 'delete-char '1)
          (list 'yas/expand-snippet '(point) '(point)
                (concat method-name template)))
      (list 'yas/expand-snippet '(point) '(point)
            (concat method-name template)))))




(defun cscomp-get-menu-item-for-clist-item (clist-item)
  "Produce a menu item for the item on the completion list.

The incoming CLIST-ITEM is either a (NAME DESCRIP) pair, or
a simple NAME.

The return value is a menu item - a thing suitable for use within
a popup menu.  It is a 2-member cons cell, the first member is a
string to be displayed, the second is the value returned when
that menu item is selected.

This value is returned to logic in
`cscomp-popup-completion-menu', and can be anything.  That
fn follows a convention that if the return value is a string, the
string is inserted. If the return value is a list, as with
ya-snippet, the list is eval'd and the result is inserted.

This fn simply transforms the simple (NAME DESCRIP) pair into
potentially something more interesting.

"
  (if (consp clist-item)
      (let ((meth-or-prop-name (car clist-item))
            (descrip (cscomp-fix-generics-in-descrip-line (cadr clist-item))))

        (cond

         ((string-match "(\\(Method\\|Constructor\\))[^(]+\\(([^)]*)\\)" descrip)
          ;; It's a method or constructor. The value returned when this
          ;; menu item is selected is a cons cell that will be eval'd.
          ;; The cons is a call to yas/expand-snippet that will insert a
          ;; template for the (possibly empty) arglist of the selected
          ;; method or constructor.
          (let ((arglist (substring descrip
                                    (match-beginning 2)
                                    (match-end 2)))
                ;; for ctor, insert the short name.
                (name-to-insert (if (string= (substring descrip
                                                        (match-beginning 1)
                                                        (match-end 1))
                                             "Constructor")
                                    (if (string-match ".+\\." meth-or-prop-name)
                                        (substring meth-or-prop-name
                                                        (match-end 0))
                                      meth-or-prop-name)
                                  meth-or-prop-name)))

            (cons (concat meth-or-prop-name " | " descrip)
                  (if (fboundp 'yas/expand-snippet)
                      (cscomp-yasnippet-for-arglist name-to-insert arglist)
                    (concat name-to-insert arglist)))))

         (t
          ;; The completion is not a method - just
          ;; insert the name of the field/prop as a string.
          (cons (concat meth-or-prop-name " | " descrip)
                meth-or-prop-name))))

    (cons clist-item clist-item)))



(defun cscomp-popup-completion-menu (title)
  "Popup a completion menu for the object at point.  The popup
menu displays all of the possible completions for the object it
was invoked on.

TITLE is the title presented.  The contents of the menu are provided
in `cscomp-current-list' .

"

  ;; cscomp-current-list holds the completion list.
  ;;
  ;; For a type, the clist gets methods, properties and fields.
  ;; Each element in the list is a list, (NAME DESCRIP), where NAME
  ;; is the name of the method/prop/field, and DESCRIP is a string
  ;; description.
  ;;
  ;; For a namespace, the clist is a list of strings.
  ;;
  ;; In each case we want to sort the list alphabetically.
  ;;

  (let* ((menu-map (cons "ignored"
                         (mapcar 'cscomp-get-menu-item-for-clist-item
                                 cscomp-current-list)))
         (menu-result (cscomp-popup-menu
                       (list (or title "Completions...") menu-map))))

    (cscomp-log 1 "menu-result:  %s" (prin1-to-string menu-result))

    ;; now, handle the selection
    (cond

     ((consp menu-result)
      (delete-region cscomp-current-beginning cscomp-current-end)
      (eval menu-result) ;; maybe a snippet expansion
      "")

     ((numberp menu-result) (number-to-string menu-result))

     ((null menu-result) "")

     (t menu-result)))) ;; a simple string






;; To do thie yasnippet thing for methods/params:
;;
;; (defun dox ()
;;   "expand a template"
;;   (interactive)
;;   (let ((template
;;    "this is ${1:index} the expanded template ${2:foo}"
;;    ))
;;   (yas/expand-snippet (point) (point) template)))




(defun cscomp-selected-frame ()
  (if (fboundp 'window-edges)
      (selected-frame)
    (selected-window)))

(defun cscomp-current-row ()
  "Return current row number in current frame."
  (if (fboundp 'window-edges)
      (+ (car (cdr (window-edges))) (count-lines (window-start) (point)))
    (count-lines (window-start) (point))))


(defun cscomp-get-point-pixel-pos ()
  "Return point position in pixels: (x, y)."
  (let ((mouse-pos  (mouse-position))
        (pixel-pos  nil)
        (ret        nil))
    (if (car (cdr mouse-pos))
        (progn
          (set-mouse-position (cscomp-selected-frame) (current-column) (cscomp-current-row))
          (setq pixel-pos (mouse-pixel-position))
          (set-mouse-position (car mouse-pos) (car (cdr mouse-pos)) (cdr (cdr mouse-pos)))
          (setq ret (list (car (cdr pixel-pos)) (cdr (cdr pixel-pos)))))
      (progn
        (setq ret '(0 0))))
    (cscomp-log 3 "mouse pos is %s" ret)
    ret))


(defun cscomp-posn-at-point-as-event (&optional position window dx dy)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of WINDOW, as a mouse-1 click
event (identical to the event that would be triggered by clicking
mouse button 1 at the top left corner of the glyph).

POSITION and WINDOW default to the position of point in the
selected window.

DX and DY specify optional offsets from the top left of the glyph."
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (unless dx (setq dx 0))
  (unless dy (setq dy 0))

  (let* ((pos (posn-at-point position window))
         (x-y (posn-x-y pos))
         (edges (window-inside-pixel-edges window))
         (win-x-y (window-pixel-edges window)))
    ;; adjust for window edges
    (setcar (nthcdr 2 pos)
            (cons (+ (car x-y) (car  edges) (- (car win-x-y))  dx)
                  (+ (cdr x-y) (cadr edges) (- (cadr win-x-y)) dy)))
    (list 'mouse-1 pos)))


(defun cscomp-popup-menu (menu-data)
  "Pop up the completion menu at point, using the data MENU-DATA.
MENU-DATA is a list of error and warning messages returned by
`cscomp-make-err-menu-data'."
  (if (featurep 'xemacs)
      (let* ((pos         (cscomp-get-point-pixel-pos))
             (x-pos       (nth 0 pos))
             (y-pos       (nth 1 pos))
             (fake-event-props  '(button 1 x 1 y 1)))
        (setq fake-event-props (plist-put fake-event-props 'x x-pos))
        (setq fake-event-props (plist-put fake-event-props 'y y-pos))
        (popup-menu (cscomp-make-xemacs-menu menu-data)
                    (make-event 'button-press fake-event-props)))
    (x-popup-menu (if (eval-when-compile (fboundp 'posn-at-point))
                      (cscomp-posn-at-point-as-event nil nil 2 20)
                    (list (cscomp-get-point-pixel-pos) (selected-window)))
                   menu-data)))




(defun cscomp-cycle-candidates ()
  "Replace the previous completion by the next one in
`cscomp-current-list'. Uses `cscomp-current-list-index'
to track the position in the list, and markers `cscomp-current-end'
and `cscomp-current-beginning' to track the position in the
buffer.

The elements in `cscomp-current-list' can be either lists, or
atoms. In the case of completing on a namespace (eg, start with System.Co),
the completion list will contain strings. (CodeDom, Console,Collections...).

If completing on a type or instance, the list elems are 2-element lists -
the car is the thing to insert, and the cadr is a description of the thing.
is it a property, method, field?  what is the return type?   if a method,
then what arguments does it require?  The cadr is used only in the menu,
not in the inserted text.

On the first entry in this function, the markers record the beginning and
end of the fragment being completed. (eg, Co if completing on System.Co).
On exit, the markers record the beginning and end of what has been inserted.
On subsequent entry, it retains those values. On all entries, this fn
deletes the text between the markers, before inserting the completion.

"
  (let (elem)
    (setq cscomp-current-list-index (1+ cscomp-current-list-index))
    (cscomp-log 3 "list index: %d" cscomp-current-list-index)
    (if (>= cscomp-current-list-index (length cscomp-current-list))
        (setq cscomp-current-list-index 0)) ;; rollover
    (setq elem (nth cscomp-current-list-index cscomp-current-list))

    (cscomp-log 3 "complete-cycle-candidates: looking at %s" (prin1-to-string elem))

    (cond
     ((listp elem)
      (if (car elem)
          (let ((thing-to-insert
                 (car elem))
                ;;(substring (car elem) (length cscomp-current-fragment)))
                )
            (delete-region cscomp-current-beginning cscomp-current-end)
            (insert thing-to-insert)
            (set-marker cscomp-current-end
                        (+ (marker-position
                            cscomp-current-beginning) (length thing-to-insert)))
            ;; display the description of the completioni n the minibuffer
            (message "cscomp: %s" (cadr elem)))
        (cscomp-log 1 (format "No completion at this point!(cycle)"))))

     ;; elem is an atom
     (t
      (let ((thing-to-insert
             elem)
            ;;(substring elem (length cscomp-current-fragment)))
            )
        (delete-region cscomp-current-beginning cscomp-current-end)
        (insert thing-to-insert)
        (set-marker cscomp-current-end
                    (+ (marker-position cscomp-current-beginning) (length thing-to-insert) ))
        ;; display the description of the completioni n the minibuffer
        (message "cscomp: %s" elem)))
     )))




(defun cscomp-complete-at-point ()
  "Completes at point.  Performs completion on field, prop or
method names if the thing being completed is a type or instance,
or on namespace members - child namespaces or types - if the
thing being completed is a namespace.

This function is interactive.

Invoking this fn multiple times in succession cycles through the
completions.

On first entry, it finds the completion, by calling out to the
CscompShell by calling `cscomp-find-completion-for-split'.
It then caches the resulting list of completion options into
`cscomp-current-list'.

On subsequent entry, it uses the cached results. After inserting a
completion, this function displays the signature of a method completion
in the minibuffer.

This fn starts the Cscompshell if necessary. Hence, you may
experience a slight delay when using this command for the first
time in a session or when completing a field or method of an
object that has many methods and fields.

See `cscomp-complete-at-point-menu' for an alternative to this command
that lets you select the desired completion from a popup menu.
"

  (interactive)

;; This command uses the Cscompshell to run Csharp code that in turn uses
;; .NET reflection to determine the methods, properties, and fields
;; defined by the type of the object at point.

  (if (and
       cscomp-current-list
       (eq last-command this-command)
       (markerp cscomp-current-beginning)
       (markerp cscomp-current-end)
       (marker-position cscomp-current-beginning)
       (marker-position cscomp-current-end)
       (>= (point) (marker-position cscomp-current-beginning))
       (eq (point) (marker-position cscomp-current-end)))

      ;; we've got a completion list.  cycle through the items.
      (cscomp-cycle-candidates)
    ;; no completion list available. get one.
    (progn
      (let* ((parse-result (cscomp-parse-csharp-expression-before-point))
             (split (and parse-result (cadr parse-result))))

        (if (not (null split))
            (progn
              ;; set markers
              (cscomp-log 1 "complete-at-point: reset begin,end to (%d,%d)"
                        (- (point) (length (cadr split)))
                        (point))

              ;; beginning: right after the dot (if there is one)
              (set-marker cscomp-current-beginning
                          (- (point) (length (cadr split))))

              (set-marker cscomp-current-end (point))
              (cscomp-find-completion-for-split split (point) (car parse-result))
              (setq cscomp-current-list-index -1)
              (cscomp-cycle-candidates)))))))




(defun cscomp-complete-at-point-menu ()
  "Pops up a menu with a list of completions for point.  When completing
on a type or instance, the menu items are the field, property and method
names; when completing on a namespace, the menu items are child namespaces
or types.

When the user makes a  selection from the menu, the selected item is
inserted at point.  In the case of a method with parameters, the inserted
thing is a parameterized snippet, a template that can then be filled in
by the user.

See `cscomp-complete-at-point' for an alternative to this function that
lets you cycle through the potential completions at point.

"
  (interactive)
  (let* ((parse-result (cscomp-parse-csharp-expression-before-point))
         (split (and parse-result (cadr parse-result))))

    ;; Reset the completion list - the thing that is used
    ;; when the "no menu" version of complete-at-pont is used.
    (setq cscomp-current-list nil)

    (if (not (null split))
        (progn
          ;; set markers
          (set-marker cscomp-current-beginning (- (point) (length (cadr split))))
          (set-marker cscomp-current-end (point))

          (cscomp-find-completion-for-split split (point) (car parse-result))

          (if cscomp-current-list
              (let* ((title  (if (car split)
                                 (concat (car split) "."
                                         (cadr split) "...")
                               (concat (cadr split) "...")))
                     (selection (cscomp-popup-completion-menu title)))
                (if (and selection
                         (not (string= selection "")))
                    (progn
                      (delete-region cscomp-current-beginning cscomp-current-end)
                      (insert selection)
                      )))
            (message "No completion at this point.")))
      (message "No completion at this point."))))




;; ;; =======================================================
;; ;; Adding a new semantic tag for "namespace".
;; ;;
;; ;; This turned out to be much harder that it should be.
;; ;;
;; ;; Apparently there are special handling for tags of type 'type,
;; ;; So, for now, punt on doing it this way.
;; ;;
;; ;; =======================================================
;; ;;
;; (defsubst semantic-tag-new-namespace (name members &rest attributes)
;;   "Create a semantic tag of class 'namespace.
;; NAME is the name of this namespace.
;; MEMBERS is a list of strings or semantic tags representing the
;; elements that are contained within the namespace.
;; ATTRIBUTES is a list of additional attributes belonging to this tag."
;;   (apply 'semantic-tag name 'namespace
;;          :members members
;;          attributes))
;;
;;
;;
;; ;; We want this:
;; ;;
;; ;; (defun semantic-tag-components-default (tag)
;; ;;   "Return a list of components for TAG.
;; ;; Perform the described task in `semantic-tag-components'."
;; ;;   (cond ((semantic-tag-of-class-p tag 'type)
;; ;;          (semantic-tag-type-members tag))
;; ;;         ((semantic-tag-of-class-p tag 'namespace)
;; ;;          (semantic-tag-type-members tag))
;; ;;         ((semantic-tag-of-class-p tag 'function)
;; ;;          (semantic-tag-function-arguments tag))
;; ;;         (t nil)))
;;
;; ;; we'll settle for this:
;;
;; (defadvice semantic-tag-components-default (after
;;                                             cscomp-advice-1
;;                                             (tag)
;;                                             activate compile)
;;
;;   (cond ((semantic-tag-of-class-p tag 'namespace)
;;          (semantic-tag-type-members tag))
;;         (t ad-return-value)))



(provide 'csharp-completion)


;;; end of csharp-completion.el
