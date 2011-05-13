;;; csharp-completion.el -- Smart code completion for C#
;;
;; Author:     Dino Chiesa <dpchiesa@hotmail.com>
;; Maintainer: Dino Chiesa <dpchiesa@hotmail.com>
;; Created:    April 2010
;; Modified:   January 2011
;; Version:    0.2
;; Keywords:   c# languages oop mode
;; X-URL:      http://code.google.com/p/csharpmode/
;;
;;
;;; Commentary:
;;
;;    Purpose
;;
;;    This is a package that performs code-completion or "intellisense"
;;    for C# within emacs buffers.  It works on GNU Emacs running on
;;    Windows and relies on Windows PowerShell in order to work.
;;
;;    Out of Scope
;;
;;    This module does not do type-ahead completion, font-lock,
;;    indenting, debugging, compiling, profiling, or management of C#
;;    project files. For other C# things, see these other modules:
;;
;;      csharp-mode.el - font-lock and indenting for C#.
;;
;;      flymake-for-csharp.el - enhancement of flymake for C#.
;;
;; -----------------------------------------------------------------------------
;;
;;    Packaging:
;;
;;    csharp-completion is delivered as a set of files:
;;
;;        CscompUtilities.dll - a .NET assembly that performs work in
;;          support of code completion. For example it can enumerate the
;;          set of types in a namespace imported by a using
;;          statement. It can also perform type inference of var types,
;;          aiding in completion for such types. Return values are
;;          delivered in the form of lisp s-expressions. The DLL is
;;          implemented in C#, and is provided with this source code,
;;          licensed under the same terms.
;;
;;        cscomp-base.el - base function, provides cscomp-log function,
;;          which is used by all other elisp modules in this
;;          package. cscomp-log is useful primarily for diagnostic
;;          purposes.
;;
;;        csharp-shell.el - runs the Powershell that loads
;;          CscompUtilities.dll and manages input to and output from the
;;          shell.
;;
;;        csharp-analysis.el - provides code analysis functions, for
;;          example the ability to enumerate all the methods in the type
;;          being defined in the current buffer; or to enumerate all the
;;          local variables that are in scope at a given point in a
;;          source code file.
;;
;;        csharp-completion.el - this module, which provides executive
;;          management of the code-completion function.  It provides the
;;          two main elisp defuns that perform code completion, as well
;;          as supporting functions for doing things like parsing the
;;          local code fragment in the buffer to determine what is being
;;          requested, popping up menus containing the list of completions,
;;          and so on.
;;
;; -----------------------------------------------------------------------------
;;
;;    External Dependencies
;;
;;    ICSharpCode.NRefactory.dll -
;;    a .NET assembly that provides sourcecode-analysis capabilities,
;;    shipped as part of SharpDevelop.  This DLL is used by
;;    CsCompUtilities.dll to do syntax analysis.
;;
;;    Windows XP, Vista, or 7
;;
;;    PowerShell 2.0 - this is included in Windows 7, free download
;;        for prior versions of Windows.
;;
;;    GNU Emacs v23.2 or later
;;
;;    cc-mode 5.31.? or later - included in Emacs 23.2
;;
;;    (optionally)  yasnippet, for snippet insertion.  If emacs has
;;    yasnippet loaded, then this completion module will insert
;;    a snippet (template) when the user selects a Method from the
;;    completion list menu.  The method snippet will have all the
;;    method parameters as placeholders; the developer then types over
;;    those placeholders to supply the actual method parameters.
;;    If yasnippet is not loaded, then the completion is just
;;    the method name, and the developer has to fill in the
;;    param-list himself. I highly recommend ya-snippet.
;;
;; -----------------------------------------------------------------------------
;;
;;    Set up
;;
;;    To set up csharp-completion:
;;
;;    1. put all the elisp files into a directory, and put that
;;       directory on your load path.
;;
;;    2. put this in your .emacs:
;;
;;       (eval-after-load "csharp-completion"
;;        '(progn
;;           (setq cscomp-assembly-search-paths
;;             (list "c:\\.net3.5ra"    ;; <<- locations of reference assemblies
;;                   "c:\\.net3.0ra"    ;; <<-
;;                     ...              ;; <<- other assembly directories you use
;;                   "c:\\.net2.0"      ;; <<- location of .NET Framework assemblies
;;                   "c:\\.net3.5"      ;; <<- ditto
;;           ))))
;;
;;       The `cscomp-assembly-search-paths' should hold a list of
;;       directories to search for assemblies that get referenced via using
;;       clauses in the modules you edit.  This will try default to
;;       something reasonable, including the "typical" .NET 2.0 and 3.5
;;       directories, as well as the default locations for reference
;;       assemblies.  If you have non-default locations for these things,
;;       you should set them here. Also, if you have other libraries (for
;;       example, the WCF Rest Starter kit, or the Windows Automation
;;       assemblies) that you reference within your code, you can include
;;       the appropriate directory in this list.
;;
;;    3. put this into your csharp-mode-hook:
;;
;;         ;; C# code completion
;;         (require 'csharp-completion)
;;         (csharp-analysis-mode 1)
;;         (local-set-key "\M-\\"   'cscomp-complete-at-point)
;;         (local-set-key "\M-\."   'cscomp-complete-at-point-menu)
;;
;;
;;
;; -----------------------------------------------------------------------------
;;
;;    Usage
;;
;;    To use code completion, open a buffer on a C# source file.  Place
;;    the cursor after a partially-completed statement, and invoke
;;    `cscomp-complete-at-point'. (Normally you would bind that fn to a
;;    particular keystroke, like M-. or M-\.) Uppn invoking that fn,
;;    this module will insert the first completion that matches. If
;;    multiple completions are possible, calling the completion function
;;    again (or pressing the bound key again) will cycle through the
;;    possibilities, similar to the way dabbrev-mode works.
;;
;;    You can alternatively call `cscomp-complete-at-point-menu', to get
;;    a popup menu of the completion choices.
;;
;; -----------------------------------------------------------------------------
;;
;;    How it Works
;;
;;    There are 2 complementary sources of information used to generate the
;;    completion options: introspection into compiled .NET class libraries
;;    (like the base class library), and source code analysis of the
;;    currently-being-edited buffer.  Both sources are facilitated by an
;;    inferior powershell shell running within emacs.  The csharp-completion
;;    module will start a powershell if it is not already running, then load
;;    into that powershell instance a custom library called CscompUtilities.
;;    (The powershell instance with the custom library loaded into it, is
;;    referred to as the CscompShell within this module.)
;;
;;    An example: if you ask for completions on the fragment "System.D",
;;    this elisp module will then call into the CscompUtilities library,
;;    by sending a command-line to the CscompShell that invokes a static
;;    method on the library, to ask the library for possible completions
;;    on the fragment.  The library builds a list of all available types
;;    from the set of referenced assemblies.  It then searches the list
;;    of types to find all possible completions for the given fragment,
;;    and prints the search result, as a string formatted as a lisp
;;    s-expression, on its output.  This elisp module then gathers the
;;    s-expression and evals it, resulting in a structure containing
;;    completion options for the given fragment.
;;
;;    In this example, starting from "System.D", the list of options
;;    returned will include the namespace System.Diagnostics as well as
;;    the type System.DateTime; this module will offer those and the
;;    other options as possible completions.
;;
;;    The latter source of information - source code analysis of the
;;    currently-being-edited buffer - is generated via the NRefactory
;;    library, which ships as part of the open-source SharpDevelop tool.
;;
;;    In the typical case, this module uses both of those sources of
;;    information, together, to generate suggestions for completions.
;;
;;    To illustrate how, consider another example: suppose your code has
;;    a local variable of type System.Xml.XmlDocument, named doc.
;;    Suppose the user asks for completion on the fragment "doc.L",
;;    implying that the user wants to reference a property, method or
;;    field on the doc variable, that begins with the letter L.  The
;;    module uses the source code analysis to identify the name and type
;;    of the local variable.  It then sends a "GetTypeInfo" command to
;;    the CscompShell, passing System.Xml.XmlDocument. The CscompShell
;;    returns a lisp s-expression enumerating the fields, methods and
;;    properties for that type.  This module then filters the
;;    s-expression for members that begin with L, and this filtered list
;;    is then used to populate the completion list, which is either
;;    displayed in a popup menu, or used to cycle the completion
;;    possibilities. The list includes LastChild, LoadXml, the four
;;    Load() overloads and LocalName.
;;
;;
;; -----------------------------------------------------------------------------
;;
;;    Here's a summary of the situations in which this module can offer
;;    completions:
;;
;;    a. names of local variables, instance variables, and method arguments.
;;
;;         int lengthOfList;
;;         void Method1(String lengthyArgumentName)
;;         {
;;            leng?
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
;;    d. M/f/p on local variable declarations with dependencies on var types:
;;
;;         var s = "This is a string";
;;         var length = s.Length;
;;         var radix = length.?
;;
;;    e. M/f/p on local variables that are initialized
;;       from instance methods and variables.
;;
;;         void method1()
;;         {
;;             var length = this.InstanceMethod();
;;             length.?
;;         }
;;
;;    f. M/f/p on generic types:
;;
;;         var x = new List<String>();
;;         x.?
;;
;;    g. constructor completion, provide template when completing
;;
;;         var x = new System.String(?
;;
;;    h. constructor completion as above, with unqualified type.
;;       This mode does a search in the namespaces from using clauses.
;;
;;         var x = new TimeSpan(?
;;
;;    i. finding types and namespaces:
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
;; -----------------------------------------------------------------------------
;;
;; things that need to be tested or fixed:
;;
;;      return new XmlS?
;;
;;      The module does not do completion on anonymous (var) types in
;;      for loops.
;;
;;      detection of presence of ya-snippet.el - how to correctly and
;;      dynamically check if ya-snippet is available, and load it if
;;      necessary?
;;
;; -----------------------------------------------------------------------------
;;
;;
;; Known bugs/problems :
;;
;;
;; TODO :
;;
;;    make an installer.
;;
;; Please send any comments, bugs, enhancements, or suggestions or
;; requests for upgrades to Dino Chiesa (dpchiesa@hotmail.com)
;;

(require 'cscomp-base)   ;; cscomp-log, etc.  basic stuff.
(require 'csharp-shell)  ;; for CscompShell, which does code introspection
(require 'csharp-analysis)  ;; for csharp-analysis-* fns


(defvar cscomp-current-p1 nil
  "The first part (before any dot) of the current completion.
This is used to determine if re-using previous results is ok.
See also `cscomp-current-list'. ")
(defvar cscomp-current-p2 nil
  "The second part (after any dot) of the current completion.
This is used to determine if re-using previous results is ok.
See also `cscomp-current-list'. ")
(defvar cscomp-current-flavor nil
  "The second part (after any dot) of the current completion.
This is used to determine if re-using previous results is ok.
See also `cscomp-current-list'. ")

(defvar cscomp-current-list nil
  "The list of all the completions for the current request. Each
element of the list is either a string, or a list, in which the car
is the possible completion, and the cadr is an additional
information about this completion.")

(defvar cscomp-current-list-index nil
  "An index to an element in cscomp-current-list. This is used to
cycle the list.")

(defvar cscomp-current-fragment nil
  "The current fragment we're trying to complete. This is used to
  trim the thing that gets inserted.")

(defvar cscomp-current-beginning (make-marker)
  "The beginning of the region where the last completion was inserted.")

(defvar cscomp-current-end (make-marker)
  "The end of the region where the last completion was inserted.")

(defvar cscomp-typeinfo-cache nil)

(defcustom cscomp-typeinfo-cache-size 150
  "The max size of completion buffer cache, in entries."
  :group 'cscomp
  :type 'integer)

(defcustom cscomp-assembly-search-paths nil
  "a list of strings, each one a directory in which to search for assemblies."
  :group 'cscomp
  :type 'list)

(defconst cscomp--new-regexp
  "^\\([ \t\n\r\f\v]*new[ \t\n\r\f\v]+\\)\\(.+\\)$")


(defun cscomp-referenced-assemblies ()
  "Return the list of .NET namespaces, each encoded as a string,
referenced in the current buffer via using statements. The result
is something like this:

 (\"System\"  \"System.Collections\"  \"System.Collections.Generic\"  \"System.Reflection\")

"
  (interactive)
  (let ((result
         (csharp-analysis-get-tagnames "import")))
    (if (called-interactively-p 'any)
        (message "result: %s" (prin1-to-string result)))
    result))



(defun cscomp--capitalize (word)
  "Capitalize WORD in place, and return it."
  (let ((first-char (aref word 0))
        result)
    (if (> first-char 96)
        (format "%c%s" (- first-char 32) (substring word 1))
      word)))



(defun cscomp--find-matching-tags (name-fragment nodeset &optional local)
  "Return the list of tags from a given set, that
match NAME-FRAGMENT.  This is used by `cscomp-matching-local-vars',
`cscomp-matching-instance-vars', and
`cscomp-matching-instance-members'.

"
  (let ((var-label (if local "(Variable) " "(Field/Prop) " ))
        result)

    (if nodeset
        (progn
          (while nodeset
            (let ((tag (car nodeset)))
              (if tag
                  (let
                      ((member-name  (csharp-analysis-tag-name tag))
                       (member-type  (csharp-analysis-tag-type tag))
                       (member-flavor (csharp-analysis-tag-flavor tag)))

              (if (eq 0 (string-match name-fragment member-name))
                  (let ((descrip
                         (cond
                          ((string= member-flavor "var")
                           (concat var-label member-type))

                          ((or (string= member-flavor "property")
                               (string= member-flavor "field"))
                           (concat "(" (cscomp--capitalize member-flavor) ") " member-type))

                          ((string= member-flavor "method")
                           (let* ((arglist (csharp-analysis-method-params tag))
                                  (modifiers (csharp-analysis-tag-modifiers tag))
                                  (arg-descrip
                                   (if (> (length arglist) 0)
                                       (concat "("
                                               (mapconcat
                                                '(lambda (x) (concat (caddr x)
                                                                     " "
                                                                     (cadr x)))
                                                arglist  ", ")
                                               ")")
                                     "()")))

                             (concat "(Method) "
                                     " " modifiers
                                     " " arg-descrip
                                     "  returns "
                                     member-type)))

                          (t ""))))

                    (cscomp-log 2 "tags: found %s (%s)"
                                member-name member-type)

                    (setq result (cons (list member-name descrip) result)))))))
            (setq nodeset (cdr nodeset)))
          (cscomp-sort-completion-list result))
      nil)))


(defun cscomp-matching-instance-vars (name-fragment)
  "Return the list of instance variables in scope, that
match NAME-FRAGMENT.  See also, `cscomp-matching-local-vars'.

"
  (interactive "sName fragment: ")
  (let ((result
         (cscomp--find-matching-tags name-fragment (csharp-analysis-instance-variables))))
    (if (called-interactively-p 'any)
        (message "result: %s" (prin1-to-string result)))
    result))




(defun cscomp-matching-instance-members (name-fragment)
  "Return the list of instance memebrs in scope in a C# module, that
match NAME-FRAGMENT.

See also, `cscomp-matching-local-vars',
`cscomp-matching-instance-vars'.

"
  (interactive "sName fragment: ")
  (let ((result
         (cscomp--find-matching-tags name-fragment (csharp-analysis-instance-members))))
    (if (called-interactively-p 'any)
        (message "result: %s" (prin1-to-string result)))
    result))



(defun cscomp-matching-local-vars (name-fragment)
  "Find the local variables currently in scope that match the given
NAME-FRAGMENT.

For the purposes of this fn, the set of \"local variables\"
includes any parameters for the method, ctor, or setter block.

See also, `cscomp-matching-instance-members',
`cscomp-matching-instance-vars'.

"
  (interactive "sName fragment: ")

  (let* ((all-vars
          (append (csharp-analysis-local-variables) ;; maybe nil
                  (csharp-analysis-local-arguments))) ;; maybe nil
         (result
          (cscomp--find-matching-tags name-fragment all-vars t)))
    (if (called-interactively-p 'any)
        (message "result: %s" (prin1-to-string result)))
    result))


(defun cscomp-type-exists (typename)
  "Determines if the type named by TYPENAME is known by the
CscompShell.  You can provide a short type name, or a
fully-qualified name.  The CscompShell must have previously
loaded the containing assembly, for it to be known.  See
`cscomp-load-one-assembly'.
"
  (interactive "sType name: ")
  (csharp-shell-invoke-shell-fn "QualifyType" typename))



(defun cscomp-produce-csharp-arglist-block-from-tag (arglist)
  "Produces an argument list block, suitable for framing within
parens in a method declaration, from ARGLIST, a list of local
arguments obtained from
`csharp-analysis-local-arguments'.

When the format of ARGLIST is like this:

  ((var \"x\" \"System.Int32\" (location (16 29) (16 33)) (id 6))
   (var \"enabled\" \"System.Boolean\" (location (16 36) (16 41)) (id 7)))

The return value is like this:

    int count, string melvin

When the arglist is empty, the return value is a string of zero length.

"
  (let ((fragment ""))
    (while arglist
      (let* ((x (car arglist))
             (var-name (cadr x))
             (var-type (caddr x)))

        (setq fragment (concat fragment var-type " " var-name)
              arglist (cdr arglist))
        (if arglist
            (setq fragment (concat fragment ", ")))))
    fragment))




(defun cscomp-produce-instance-member-code-fragment (member-list)

"Generate a fragment of C# source code that can be compiled to
produce IL, which can then be inspected to determine the type of
the var for which the user is asking completion.

This fn generates dummy code that includes placeholders for all
instance members, method arguments, and local variables. The
generated code is then compiled, so that Cscomp can inspect the
resulting IL to determine the type of a local var on which the
user is asking for completion.

Cscomp uses the compiler to determine the type of the var. It
dynamically generates and compiles a class with the same variable
declaration as the code being edited.

The initialization of the var may depend on instance members.  If
so, the compiled class must provide instance members of the
correct type to satisfy those dependencies.  This function
generates C# code to provide those instance members.

For input that looks like this:

  ((var \"staticField1\" \"System.Int32\"
     (modifier \"private static\")
     (location (14 9) (14 20)) (id 17))

   (method \"InstanceMethod1\" \"System.String\"
     (modifier \"public\")
     (params
       (var \"x\" \"System.Int32\" (location (16 29) (16 33)) (id 6))
       (var \"enabled\" \"System.Boolean\" (location (16 36) (16 41)) (id 7)))
     (block ...)
     (location (16 9) (18 10)) (id 9))
    ...
   )

The output will look like this:

    class foo {
      private static int staticField1 = default(int);
      string InstanceMethod1(...) { return default(string); }
      ...
    }

Any void methods will not be emitted because they cannot affect the
types of local variables declared in methods.

"
  (let ((synthetic-code ""))

    (while member-list
      (let* ((x (car member-list))
             (member-name   (csharp-analysis-tag-name x))
             (member-flavor (csharp-analysis-tag-flavor x))
             (member-type   (csharp-analysis-tag-type x))
             (member-modifiers (csharp-analysis-tag-modifiers x))
             one-frag )

        (setq one-frag
              (cond

               ((or
                (string= member-type "void") ;; the member is a void type
                (string= member-type "System.Void"))
                "")                          ;; emit nothing, don't need it.

               ;; if it turns out I was wrong, and the fn is necessary,
               ;; we can just emit an empty fn.

               ((string= member-flavor "method") ;; it's a method
                (concat member-modifiers " " member-type " "
                        member-name "(" (cscomp-produce-csharp-arglist-block-from-tag
                                         (csharp-analysis-method-params x))
                        ") {"
                        (if member-type
                            (concat
                             " return default("
                             member-type
                             ");")
                          "")
                        "} "))

               ((or
                (string= member-flavor "field") ;; it's an instance field
                (string= member-flavor "property")) ;; it's an instance property

                (concat
                 member-modifiers " " member-type " "
                 member-name " = default(" member-type "); "))

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
  (let ((start-pos 0))

    (while (string-match "\\([^']*\\)'\\(.*\\)" s start-pos)
      (setq s (concat
               (substring s 0 start-pos)
               (substring s start-pos (match-end 1))
               "''"
               (substring s (+ 1 (match-end 1))))
            start-pos (+ 2 (match-end 1)))))
  s)


(defun cscomp--infer-type-given-decl (var-declaration
                                       var-index
                                       classname
                                       arglist
                                       instance-members)
  "Determines the type of the var declared in the given declaration.
VAR-DECLARATION is the C# code that declares all the local vars up to
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

  (let ((pathlist
         (mapconcat 'identity cscomp-assembly-search-paths ",")))
    (csharp-shell-invoke-shell-fn "SetAssemblySearchPaths" pathlist))


  (let* ((massaged-decl
          (cscomp-escape-single-quotes
           (cscomp-consolidate-whitespace var-declaration)))
         (namespaces (mapconcat 'identity (cscomp-referenced-assemblies) ","))
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
    (csharp-shell-do-shell-fn command-string)))




(defun cscomp-get-completions-for-namespace (ns)
  "Gets a list of known completions (types and child namespaces)
for the given namespace. You must have loaded an assembly containing
types from the namespace, into the shell, for it to be known.
See `cscomp-load-one-assembly'."
  (interactive "sNamespace: ")
  (csharp-shell-invoke-shell-fn "GetCompletionsForNamespace" ns))



(defun cscomp-qualify-type-name (type-name)
  "returns fully-qualified type name for a given short TYPE-NAME.

Return value is a string, or nil if the type-name is unknown.

Example:  (cscomp-qualify-type-name \"XmlAttribute\")
returns   \"System.Xml.XmlAttribute\"

"
  (let ((result (cscomp-qualify-name type-name)))
    (cond
     ((string= (nth 0 result) "type")
      (nth 1 result))
     (t
      nil))))



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
  (csharp-shell-invoke-shell-fn "QualifyName" name))


(defun cscomp-get-all-known-matches (fragment)
  "returns a list of all possible matches on a given partial name.
The return value is like this:

   ((\"type\"  \"fullNameOfType\")
    (\"namespace\"  \"FullNameOfNamespace\")
    (\"method\"  \"NameOfMethod\")
    (\"variable\"    \"woof\" \"(local) System.Int32\"))

"
  (interactive "sfragment to match on: ")

  (let* ((fixup-varlist
          (lambda (item)
            (list "variable" (car item) (cadr item))))
         (fixup-instancelist
          (lambda (item)
            (list "instance" (car item) (cadr item))))
         (instance-vars (mapcar fixup-instancelist
                                (cscomp-matching-instance-vars fragment)))
         (locals (mapcar fixup-varlist
                         (cscomp-matching-local-vars fragment)))
         (namespaces (mapconcat 'identity (cscomp-referenced-assemblies) ","))
         (matches (csharp-shell-do-shell-fn
                   (concat "[Ionic.Cscomp.Utilities]::GetMatches('"
                           fragment
                           "','"
                           namespaces
                           "')"))))
    (append instance-vars locals matches)))


(defun cscomp-load-one-assembly (lib)
  "Loads a assembly into the csharp-shell, which then allows
Cscomp to do completion (etc) on the types in that library. The
assembly should be a namespace of an assy that is in the GAC, or
the full path of the assembly file (usually a DLL).
"
  (interactive "sLibrary: ")
  (csharp-shell-invoke-shell-fn "LoadOneAssembly" lib))




(defun cscomp-load-imported-namespaces ()
  "Loads assemblies for the imported namespaces into the CscompShell."
  (mapcar 'cscomp-load-one-assembly
          (cscomp-referenced-assemblies)))



(defun cscomp-get-qualified-name (name)
  "Guess the fully qualified name of the class NAME, using the
list of referenced assemblies. It returns a string if the fqn
was found, or null otherwise."
  (interactive "sType name: ")
  (cscomp-log 2 "get-qualified-name (%s)" name)
  (let ((result (cscomp-type-exists name)))
    (if result result
      ;; else
      (let ((usinglist (cscomp-referenced-assemblies))
            fullname namespace )

        ;; usinglist is like this:
        ;; ("System"  "System.Collections"  "System.Collections.Generic" ...)

        (setq result nil)
        (while usinglist
          (setq namespace (car usinglist))
          (cscomp-load-one-assembly namespace)
          (setq fullname (concat namespace "." name))
          (cscomp-log 2 "checking this type: '%s'" fullname)
          (if (cscomp-type-exists fullname)
              (setq result fullname
                    usinglist nil)  ;; end the loop
            (setq usinglist (cdr usinglist)))))

        (cscomp-log 2 "get-qualified-name (%s): '%s'" name result)

        result)))


(defun cscomp-reset-typeinfo-cache ()
  "Clears all entries in the type information cache."
  (interactive)
  (setq cscomp-typeinfo-cache nil))


(defun cscomp-add-to-typeinfo-cache (name typeinfo)
  (let (new-entry new-list)
    (if (nth cscomp-typeinfo-cache-size cscomp-typeinfo-cache)
        (progn
          (setq new-entry (list name typeinfo))
          (setq new-list (list new-entry nil))
          (setcdr new-list (cdr cscomp-typeinfo-cache))
          (setq cscomp-typeinfo-cache new-list)
          (cscomp-log 1 "typeinfo cache is full"))
      ;;else
      ;; nconc?
      (setq cscomp-typeinfo-cache
            (append
             cscomp-typeinfo-cache
             (list (list name typeinfo)))))))


(defun cscomp--get-typeinfo-from-cache (name usinglist)
  "Gets type information for the type with NAME, from the cache."
  (if (string= name "var")
      nil ;; unknown
    (let ((temp (nth 0 cscomp-typeinfo-cache))
          (index -1)
          (found nil))
      (while (and temp (not found))
        (incf index)
        (cscomp-log 4 "looking at cache item %d" index)
        (setq temp (nth index cscomp-typeinfo-cache))
        (if (string= (car temp) name)
            (setq found t)))
      (if found
          (progn
            (cscomp-log 3 "-get-typeinfo-from-cache: HIT name(%s) r(%s)"
                        name (prin1-to-string (nth 1 temp)))
            (nth 1 temp))
        (cscomp-log 3 "-get-typeinfo-from-cache: MISS name(%s)" name)
        nil))))



(defun cscomp-get-typeinfo (name)
  "Return the class info list for the class NAME. This function first
checks to see if the class info is cached. If so, it returns the
cached class info. Otherwise, it creates the type info list. Each
element of the list returned by this function is itself a list whose
car is a possible completion and whose cdr gives additional
informations on the completion - the property type, or the param
list and return type for a method, etc."
  (interactive "sTypename: ")

  (if (string= name "var")
      (progn
        (cscomp-log 2 "ENTER get-typeinfo name(%s)...result:nil" name)
        nil) ;; unknown

    (cscomp-log 2 "ENTER get-typeinfo name(%s)...trying cache..." name)

    (let* ((usinglist (cscomp-referenced-assemblies))
           (type-info (cscomp--get-typeinfo-from-cache name usinglist))
           (ulist  (mapconcat 'identity usinglist ", "))
           namespace
           qualified-type )

      ;; load all the assemblies mentioned in the using clauses
      (while usinglist
        (setq namespace (car usinglist))
        (cscomp-load-one-assembly namespace)
        (setq usinglist (cdr usinglist)))

      (if (null type-info)
          (progn
            (setq qualified-type
                  (csharp-shell-exec-and-eval-result
                   (concat
                    "[Ionic.Cscomp.Utilities]::QualifyType('"
                    ;;(cscomp-escape-string-for-powershell name)
                    ;; dont need to escape if using single quotes
                    name
                    "','"
                    ulist
                    "')")))

            (cscomp-log 2 "get-typeinfo...(%s)..." (prin1-to-string qualified-type))

            (if qualified-type
                (setq type-info
                      (csharp-shell-exec-and-eval-result
                       (concat "[Ionic.Cscomp.Utilities]::GetTypeInfo('"
                               (car qualified-type) ; type name
                               "', '"
                               (cadr qualified-type) ; assembly name
                               "')" ))))
            (if type-info
                (cscomp-add-to-typeinfo-cache name type-info))))

      type-info)))


(defun cscomp-get-type-ctors (type-name)
  "Retrieve constructors from CscompShell for the type named by TYPE-NAME.
"
  (interactive "sType name: ")
  (csharp-shell-invoke-shell-fn "GetConstructors" type-name))




(defun cscomp-split-by-dots (s)
  "When the string contains a dot, this fn returns a 2-element
list (TOKEN1 TOKEN2).  TOKEN1 is the substring of s that precedes
the last dot.  TOKEN2 is the substring that follows the last dot.

When the string does not contain a dot, this fn returns a
2-element list in which the first element is nil and the 2nd
element is the entire string.

"
  (cscomp-log 2 "split-by-dots: s[%s]" s)
  (if (string-match "\\(.*\\)\\.\\(.*\\)" s)
      (let ((result (list (match-string 1 s) (match-string 2 s))))
        (cscomp-log 2 "split-by-dots: %s" (prin1-to-string result))
        result)
    (list nil s))) ;; it's a single atom




(defun cscomp-parse-csharp-expression-before-point ()
  "Parses the text at point, and returns the results.
 Side effect: sets markers to the beginning
 (cscomp-current-beginning) and end (cscomp-current-end)
 of the parsed expression.

The return value is a list (POSN (TOKEN1 TOKEN2)) , where POSN is
the position in the buffer of the beginning of TOKEN1 and TOKEN1
and TOKEN2 are the two tokens surrounding the prior dot.

Some examples.

#1
Suppose System.Diagnostics.D is the text that appears prior to
point. This function would return the list

    (888 (\"System.Diagnostics\" \"D\"))

...if 888 was the position of the beginning of the word System.


#2
Suppose new System.DateTime the text that appears prior to
point. This function would return the list

    (721 (nil \"new System.DateTime\"))

...if 721 is the position of the beginning of the word \"new\".

---

It's just an exercise in syntactically-aware string parsing.  It
uses some cc-mode magic to do some of that.

If the text preceding point doesn't look like two tokens, this fn
returns nil, and does not set the markers.

"
  (interactive)

  (cscomp-log 2 "parse-csharp-expression-before-point: point(%d)" (point))

  (save-excursion
    (let ((opoint (point))
          (cycle-count 0)
          (dot-count 0)
          (regex "[-\\+\\*\\/%,;|( {})=\\.]")
          snip done m1
          (paren-depth 0)
          result)

      ;; Repeatedly step backward, over whitespace and c# source tokens.
      ;; The regex above defines what we skip back TO.  The first cycle
      ;; through, we skip back to any of the set of
      ;; syntactically-relevant symbols including + - * / % , ; | ( ) {
      ;; } = or dot (.).  If the first step backwards finds a dot, we change the
      ;; regex to find anything in that list EXCEPT the dot.
      ;; Stepping backwards twice gives us a pair of tokens, which we can then
      ;; use to perform completion on .

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
            (if (not m1) (setq m1 (point)))
            t)                      ;; do nothing else

           ;; The char after is a dot.  Ergo, change the regex.
           ;; The next step back will not stop on dot.
           ((eq (char-after) ?.)
            (setq m1 (point))
            (setq regex "[-\\+\\*\\/%,;|( {})=]"))

           (t
            (setq done t))))

         (t
          (backward-char)))

        (incf cycle-count)) ;; count of steps backward

      ;; set markers, before consolidating whitespace
      (let ((beg (1+ (or m1 (point)))))

        (cscomp-log 2 "parse-csharp-expression-before-point: reset begin,end to (%d,%d)"
                    beg opoint)

        ;; beginning: right after the dot (if there is one)
        (set-marker cscomp-current-beginning beg)
        (set-marker cscomp-current-end opoint ))

      (setq snip
            (cscomp-consolidate-whitespace
             (buffer-substring-no-properties (1+ (point)) opoint)))

      (cscomp-log 2 "parse-csharp-expression-before-point: B snip(%s)" snip)
      (setq result (list (1+ (point))
                         (if (eq 0 (string-match "new " snip))
                             (list nil snip)
                         (cscomp-split-by-dots snip))))

      (if (called-interactively-p 'any)
          ;; If called interactively, show the result at
          ;; the bottom of the screen.
          (message "result: %s" (prin1-to-string result)))

      result)))




(defun cscomp--random-symbol-name ()
  (random t)
  (loop
   for i below 10
   collect (+ (if (= (random 2) 0) 65 97) (random 26)) into auth
   finally return (concat auth)))

(defun cscomp-last-char-in-string (s)
  (let ((ix (length s)))
    (elt s (- ix 1))))



(defun cscomp--infer-type-of-symbol (symbol initializers args)
  "A helper fn. This infers the type of a given SYMBOL, assuming
the provided variable initializers (INITIALIZERS) and method
arguments (ARGS) that may be used to initialize the named symbol.

The return value is a list (VAR-TYPE VAR-POSN) , where VAR-TYPE
is a string, a typename, and VAR-POSN is a list of (START FINISH)
that describes where in the buffer the variable was declared.  In
the case of contrived variables, which are used to infer the type
of arbitrary expressions, the VAR-POSN is not valid.

"
  (let* ((result nil)
        (decl-count 0)
        (brace-count 0)
        (prior-var-decls "")
        (containing-type (csharp-analysis-current-class)))

    ;; look at local args first
    (loop
     for x in args do
     (if (not result)
         (progn
           (cscomp-log 3 "infer-type-of-symbol: looking at arg (%s)"
                       (prin1-to-string x))
           (let ((var-name (cadr x)))
             ;; If this arg decl is the same name as the one
             ;; we want, things are simple. We know the type.
             (if (string= var-name symbol)
                 (let
                     ((var-type (caddr x))
                      (var-posn          ;; pair: start and end of var decl
                       (mapcar 'cscomp-pos-at-line-col
                               (cdr (csharp-analysis-tag-location x)))))
                   (cscomp-log 3 "infer-type-of-symbol: found arg %s (%s)"
                               symbol var-type)
                   (setq result (list var-type var-posn))))))))


    ;; now look at local variables
    (while (and initializers (not result))
      (let* ((x (car initializers))
             (var-name (cadr x))
             (var-id   (csharp-analysis-tag-id x))
             (var-type (caddr x))
             (var-posn          ;; pair: start and end of var decl
              (mapcar 'cscomp-pos-at-line-col
                      (cdr (csharp-analysis-tag-location x)))))

        (cscomp-log 3 "infer-type-of-symbol: looking at var (%s)"
                    (prin1-to-string x))

        ;; If this var decl is the same name as the one
        ;; we want.
        (if (string= var-name symbol)
            (progn
              ;; Handle var types - need to determine the actual type.
              ;;
              ;; To do that, compile the var declaration, then inspect the IL
              ;; to determine the var types.
              ;;
              ;; This engine will determine the var type, in some portion of
              ;; the cases.
              ;;
              ;; It will handle:
              ;;
              ;;  - simple var declarations that have no dependencies on other vars
              ;;  - cascaded var decls that depend on other local vars.
              ;;  - var whose initializaiton depends on a method argument
              ;;  - var whose initialization depends on an instance var
              ;;  - var decls in foreach loops
              ;;  - var decls in using blocks
              ;;

              ;; if the type of the variable is "var"
              (if (string= var-type "var")
                  (progn
                    (let* ((member-list (csharp-analysis-class-members containing-type))
                           (name-of-containing-type  (cadr containing-type))
                           (parent-tag
                            (csharp-analysis-find-parent-tag-by-id-from-ast (list containing-type) var-id))
                           (parent-flavor (and parent-tag (csharp-analysis-tag-flavor parent-tag)))
                           (var-init   (assoc 'initialvalue x))
                           (this-decl
                            (cond
                             ((string= parent-flavor "foreach")
                                (concat
                                 "foreach (var "
                                 var-name
                                 " in "
                                 (buffer-substring-no-properties (elt var-posn 0) (elt var-posn 1))
                                 ") {}"))  ;; handle foreach

                             (var-init ;; for contrived variables
                                (concat
                                 "var " var-name " = " (cadr var-init) ";"))

                              (t
                               (cscomp-log 3 "not a foreach loop.")
                               (let ((frag
                                      (buffer-substring-no-properties
                                       (elt var-posn 0) (elt var-posn 1))))
                                 (if (not (string-match ";[ \t]*$" frag))
                                     (progn
                                       (message "appending semi to frag: %s" frag)
                                       (concat frag ";"))
                                   (progn
                                       (message "var decl (no semi needed): %s" frag)
                                       frag))))))
                           inferred-type)

                      ;; append close-braces for any open for or foreach loops
                      (while (> brace-count 0)
                        (setq brace-count (- brace-count 1)
                              this-decl (concat this-decl "}")))

                      ;; Infer the type. This generates C# code, compiles it,
                      ;; and then inspects the IL to determine the type.
                      (setq inferred-type
                            (cscomp--infer-type-given-decl
                             (concat prior-var-decls " " this-decl)
                             decl-count
                             name-of-containing-type
                             (cscomp-produce-csharp-arglist-block-from-tag args)
                             (cscomp-produce-instance-member-code-fragment member-list)))

                      (if (and inferred-type (string= (car inferred-type) "type"))
                          (setq var-type (cadr inferred-type))
                        (message "%s" (prin1-to-string inferred-type))
                        ))))

              (cscomp-log 2 "infer-type-of-symbol: result %s.GetType() = (%s)"
                          symbol var-type)
              (setq result (list var-type var-posn)))

          ;; else (it's not the variable we want)
          ;; Remember it. It may be necessary to initialize our var of interest.
          (let ((this-var-decl
                 (buffer-substring-no-properties (elt var-posn 0) (elt var-posn 1)))
                emitted)

            (if (string= var-type "var")
                (let* ((containing-type (csharp-analysis-current-class))
                       (parent-tag
                        (csharp-analysis-find-parent-tag-by-id-from-ast
                         (list containing-type)
                         var-id))
                       (parent-flavor (and parent-tag (csharp-analysis-tag-flavor parent-tag))))

                  (if (string= parent-flavor "foreach")
                      ;; its a var in a foreach
                      (setq brace-count (1+ brace-count)
                            emitted t
                            prior-var-decls (concat prior-var-decls
                                                    " foreach (var "
                                                    var-name
                                                    " in "
                                                    this-var-decl
                                                    ") {\n")))))
            (if (not emitted)
                ;; include this decl in the set of prior decls
                (progn
                  (setq prior-var-decls (concat prior-var-decls " " this-var-decl)
                        decl-count (1+ decl-count))
                  ;; add a semicolon if necessary. This happens in
                  ;; case of a using clause, for example.
                  (if (/= (cscomp-last-char-in-string this-var-decl) 59)
                      (setq prior-var-decls (concat prior-var-decls ";")))
                  )))))

      (setq initializers (cdr initializers)))

    result))




(defun cscomp-infer-type-of-expression (expr)
  "Use the sourcecode analysis results to infer the type of the
given expression, EXPR, which must be a string. Returns a
typename, or nil if unknown.

To infer the type, this fn generates C# source code that includes
the expression in question, compiles that code, and inspects the
IL to determine the type of the expression.

The return value is the ???

See also `cscomp-qualify-local-var'.
"

  ;; To generate compilable code, we need to include all relevant
  ;; variables and arguments, that the expression in question may depend
  ;; upon. So, iterate through arguments and local variables, inject all
  ;; of them into a compilable source fragment, then pass that fragment
  ;; off to the CscompUtilities assembly to compile it and inspect the
  ;; result.
  ;;

  (cscomp-log 3 "ENTER infer-type-of-expr: (%s)" (or expr "--"))

  (let* ((contrived-name (cscomp--random-symbol-name))
         (contrived-var
          (list 'var contrived-name "var"
                (list 'initialvalue expr) ;; the infererence logic will use this
                '(location (1 1) (1 2))   ;; dummy
                '(id 0)))
         (initializers
          (append (csharp-analysis-local-variables) (list contrived-var)))
        (args (csharp-analysis-local-arguments)))
    (cscomp--infer-type-of-symbol contrived-name initializers args)))




(defun cscomp-qualify-local-var (symbol)
  "Use the sourcecode analysis results to classify the
SYMBOL as a local variable.

If the symbol is a local variable, this fn returns a list, 1st
elt is the variable type, and the second elt is a vector,
containing the position of its definition.

If the symbol is not recognized as a local variable, this fn
returns nil.

See also `cscomp-qualify-instance-var'.
"
  (cscomp-log 3 "ENTER qualify-local-var: (%s)" (or symbol "--"))
  (if (or
       (null symbol)
       (eq 0 (string-match cscomp--new-regexp symbol)))
      (progn
        (cscomp-log 3 "EXIT qualify-local-var: (%s)" (or symbol "--"))
        nil)
  (let ((locals (csharp-analysis-local-variables))
        (args (csharp-analysis-local-arguments)))
    (cscomp--infer-type-of-symbol symbol locals args))))




(defun cscomp-pos-at-line-col (position)
  "Return the char position at the given line/column POSITION,
which is of the form (LINE . COLUMN).

"
  (let ((line (nth 0 position))
        (column (nth 1 position)))
    (save-excursion
      (goto-char (point-min))
      (forward-line (- line 1))
      (+ (point) (- column 1)))))




(defun cscomp-qualify-instance-var (symbol)
  "Use the csharp source analysis results to classify the
name as an instance variable. Returns a list, 1st elt is the
variable type, and the second elt is a vector, containing
the position of its definition.

See also `cscomp-qualify-local-var'.

"
  (if (or
       (null symbol)
       (eq 0 (string-match cscomp--new-regexp symbol)))
      nil
  (let ((ivars (csharp-analysis-instance-variables))
        (result nil))
    (while (and ivars (not result))
      (let* ((x (car ivars))
             (var-name (cadr x))
             (var-type (caddr x))
             (var-posn          ;; pair: start and end of var decl
              (mapcar 'cscomp-pos-at-line-col
                      (cdr (csharp-analysis-tag-location x)))))

        (if (string= var-name symbol)
            (progn
              (cscomp-log 2 "qualify-instance-var: found %s (%s)"
                       symbol var-type)
            (setq result (list var-type var-posn))))
      (setq ivars (cdr ivars))))
    result)))




(defun cscomp-completion-list-for-ns (nsinfo)
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
   \"Something1 | (namespace)\"
   \"Something2`2 | (namespace)\")

It's just a transformation from one data format to another.

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
      (incf n)))
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





(defun cscomp-completion-list-for-type (typeinfo)
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

    (cscomp-log 3 "completion-list-for-type: typename(%s)" tname)

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


    (cscomp-log 2 "completion-list-for-type: result: "
              (prin1-to-string result))
    ;;(print result (get-buffer "*Messages*"))
    result))



(defun cscomp-completion-list-for-ctors (ctor-info)
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
        (tname (car ctor-info))
        (ctors (cadr (caddr ctor-info))))

    (cscomp-log 2 "completion-list-for-ctors: typename(%s)" tname)

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
                       (list tname  ;; name of the ctor
                             ;; description for this ctor
                             (concat "(Constructor) "
                                     modifiers  ;; modifiers on this prop
                                     " "
                                     params
                                     )))
                      result)))
      (setq ctors (cdr ctors)))

    (cscomp-log 3 "completion-list-for-ctors: result: "
                (prin1-to-string result))
    result))



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



(defun cscomp-find-all-ns-completions (fragment lst &optional exact-match )
  "Find all completions for FRAGMENT in LST.  If EXACT-MATCH is true,
then...?  LST is a simple list of strings, as obtained from
`cscomp-completion-list-for-ns'.
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

LST is a list obtained from `cscomp-completion-list-for-type'.

If STATIC is non-nil, then match only non-static props/fields/methods.
Otherwise, match only static fields/props/methods.

If EXACT-MATCH is true, then only the completions that match the
FRAGMENT exactly are returned.  Normally, EXACT-MATCH is not
true, and in thi case, the completions that start with the
FRAGMENT are returned.

The result is...a simple list of strings.

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

        (cscomp-log 4 "find-all-type-completions, looking at %s %s"
                  (prin1-to-string candidate)
                  (if is-match "MATCH" ""))

      (if is-match
            (setq result (append (list candidate) result))))

      (setq lst (cdr lst)))

    (cscomp-log 3 "find-all-type-completions, result: %s"
                (prin1-to-string result))
      ;;(print result (get-buffer "*Messages*"))

    (setq cscomp-current-fragment fragment)
    (cscomp-sort-completion-list result)))




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
  "Maps the primitive type \"short name\" to the expanded name of
the type, and returns that expanded name. For example, \"bool\"
maps to \"System.Boolean\" .

If type is not a primitive type, the result is nil.
"

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
            ;;else
            (setq lst (cdr lst))))
        result )
    ;;else
    nil ))

(defun cscomp-find-completion-for-split (split beginning-of-token1 &optional reuse-results)
"Find the possible completions for a given string, which has
been previously parsed. This is where the main work of the module
is performed.

SPLIT is a list of (TOKEN1 TOKEN2), as returned from
`cscomp-split-by-dots'.

If the user has typed something followed by a dot, optionally
followed by an fragment after the dot, and then asked for
completions via `cscomp-complete-at-point-menu' or
`cscomp-complete-at-point', then this function gets invoked, with
TOKEN1 containing what precedes the dot, and TOKEN2 containing
what follows the dot. For example, TOKEN1 may be a C# namespace,
a class name, a variable name (local variable or a method or
property or field name on the current class), or explicitly,
\"this\" .  TOKEN2 may be the empty string, or a fragment of a
name, corresponding to a method property or field belonging to
the thing represented by the first token.  The goal is to find a
completion for the second token, given the context of the first
token.

The return value is a list. For example, if the user types the
name of an int variable, then a dot, then asks for completions,
the return value is:

  ((\"CompareTo\" \"(Method) sealed public  (System.Int32 value)  returns System.Int32\")
   (\"CompareTo\" \"(Method) sealed public  (System.Object value)  returns System.Int32\")
   (\"Equals\" \"(Method) sealed public  (System.Int32 obj)  returns System.Boolean\")
   (\"Equals\" \"(Method) public  (System.Object obj)  returns System.Boolean\")
   (\"GetHashCode\" \"(Method) public  ()  returns System.Int32\")
   (\"GetType\" \"(Method) public  ()  returns System.Type\")
   (\"GetTypeCode\" \"(Method) sealed public  ()  returns System.TypeCode\")
   (\"ToString\" \"(Method) sealed public  (System.String format, System.IFormatProvider provider)  returns System.String\")
   (\"ToString\" \"(Method) sealed public  (System.IFormatProvider provider)  returns System.String\")
   (\"ToString\" \"(Method) public  ()  returns System.String\")
   (\"ToString\" \"(Method) public  (System.String format)  returns System.String\"))


Another Example: if the user types System.Diag and asks for completions,
then SPLIT will be (\"System\" \"Diag\") and the returned completion
list will contain \"Diagnostics\".

If the user has simply typed a string, and asked for completions,
then TOKEN1 will be nil and TOKEN2 will contain the string.
This fn then finds possible completions for that single string.

"
  (cscomp-log 2 "find-completion-for-split: A: '%s' '%s'"
            (car split) (cadr split))


  ;; In most cases, p1 is what precedes the final dot, p2 is what
  ;; follows.  When there is no dot, p1 is nil.  when this is an
  ;; invocation of a constructor, even if the typename contains a dot,
  ;; then p1 is nil and p2 contains "new ZZZ" where ZZZ is the typename,
  ;; with any qualifying namespace.
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
  ;;    that matches.
  ;;
  ;; 5. p1 is nil, and p2 is "new ZZZ", where ZZZ is the partial name of a type.
  ;;    In this case, complete with the name of a type or namespace
  ;;    that matches the ZZZ portion of p2.
  ;;
  ;; 6. p1 is the name of a type, like System.Console, and p2 is something.
  ;;    In this case, complete with static field, prop, and methods on
  ;;    that type.
  ;;
  ;; 7. p1 is the name of a primitive type, like byte or int.
  ;;    In this case, replace it with the expanded type, and complete as
  ;;    in case 5.
  ;;
  ;; 8. others?
  ;;
  ;; =============================================
  ;;
  ;; There are two sources for completion possibilities.  For known types,
  ;; the possibilities come from introspection, done through the CscompShell.
  ;; So, for System.Xml.XmlDocument, we send a message to the shell and get back
  ;; all the known fields/props/methods of that .NET type. For System.Xml.X, we
  ;; send a message to the CscompShell and get back a list of types and namespaces
  ;; that match.
  ;;
  ;; For unknown types - currently the only "unknown" qualifier is "this." -
  ;; use the source code analysis results to find fields/props/methods.
  ;;

  (let* ((p1 (car split))  ;; part 1 - usually what precedes the dot
         (p2 (cadr split)) ;; part 2 - what comes after the dot (if anything)
         (is-primitive    (cscomp-map-primitive-type p1))
         (is-local-var    (and (not is-primitive) (cscomp-qualify-local-var p1)))
         (is-instance-var (and (not is-primitive)
                               (not is-local-var)
                               (cscomp-qualify-instance-var p1)))
         (is-func         (string-match "($" p2))  ;; p2 ends in open-paren
         (is-ctor         (and                     ;; constructor of bare type?
                            (null p1)
                            (eq 0 (string-match cscomp--new-regexp p2))))
         cflavor p1-type is-static r)


    ;; figure out what we're completing on.

    (cond
     (is-ctor
      (cscomp-log 3 "find-completion-for-split: is-ctor")
      ;; user may be asking for completions on:
      ;;    - a namespace name
      ;;    - a type name within a namespace
      ;;    - a constructor.  This is sort of like a type name except,
      ;;      the p2 token ends in an open-paren.
      (setq cflavor (if is-func "ctors" "namespace")
            p2 (substring p2 (match-beginning 2)) ;; trim the "new"
            split (cscomp-split-by-dots p2)
            p1 (car split)
            p2 (cadr split)))

     (is-primitive
      ;; A method/prop/field on a byte, int, char, etc.
      (cscomp-log 3 "find-completion-for-split: is-primitive")
      (setq cflavor "type"
            p1-type is-primitive
            is-static t)) ;; get static methods/props/fields on System.Byte, etc

     (is-local-var
      ;; looking for a completion (method, property, or field) on a local variable.
      (cscomp-log 3 "find-completion-for-split: is-local-var %s" (prin1-to-string is-local-var))
      (setq cflavor "type"
            p1-type (or (cscomp-map-primitive-type (car is-local-var))
                        (car is-local-var))))

     (is-instance-var
      ;; it's an instance variable.
      (cscomp-log 3 "find-completion-for-split: is-instance-var")
      (setq cflavor "type"
            p1-type (or (cscomp-map-primitive-type (car is-instance-var))
                        (car is-instance-var))))

     ((string= "this" p1)
      ;; complete on instance field/prop/method
      (setq cflavor "this"))

     ;;; this can never happen.
     ;;;
     ;; ((and (null p1) ;; no prefix.
     ;;       is-func) ;; open paren is the last char, eg  "new TimeSpan(?"
     ;;  (setq r (cscomp-qualify-name (substring p2 0 -1)))
     ;;  (cond ((listp r)
     ;;         (if (string= (car r) "type")
     ;;             (setq cflavor "namespace" ;; TODO: verify correctness
     ;;                   p1-type (and
     ;;                            (string-match "^\\(.+\\)\\..+$" (cadr r))
     ;;                            (match-string 1 (cadr r)))
     ;;                   p1 p1-type)
     ;;           (setq cflavor "local")))
     ;;        (t
     ;;         (setq cflavor "local"))))

     ((null p1) ;; no prefix
      (cond
       (is-func
        (error "not sure what to do here."))
       (t
        (setq cflavor (if (or (not p2) (string= p2 ""))
                          "nothing" ;; nothing to do
                        "mixed")))))

     (t
      (setq r (cscomp-qualify-name p1))
      (cond
       ((listp r)
        (progn
          (setq cflavor (car r))

          (if (string= cflavor "unknown")
              ;; assume p1 is an expression.  Try to infer the type.
              (let ((inferred-type-info
                     (cscomp-infer-type-of-expression p1)))

                (if inferred-type-info
                    (setq p1-type
                          (or (cscomp-map-primitive-type (car inferred-type-info))
                              (car inferred-type-info))
                          cflavor "type")))

            ;; name qualification was successful
            (setq p1-type p1
                  is-static t)))) ;; if it's a type, we want static f/m/p only

       (t
        (error "qualify-name returns invalid results.")))))


    (cscomp-log 2 "find-completion-for-split: B: p1(%s,%s) p2(%s) flav(%s)"
              p1 (if p1 p1-type "--")  (prin1-to-string p2) cflavor)

    (if (and reuse-results
             cscomp-current-list
             (and cscomp-current-flavor (string= cflavor cscomp-current-flavor))
             (and cscomp-current-p1 (string= p1 cscomp-current-p1))
             (and cscomp-current-p2 (cheeso-string-starts-with p2 cscomp-current-p2)))
        (progn
          (setq cscomp-current-p2 p2)
          (cscomp-log 2 "find-completion-for-split: re-using previous result (%s %s)"
                      p1 p2)
          cscomp-current-list)

      (progn
        ;; store these for future use
        (setq cscomp-current-p1 p1
              cscomp-current-p2 p2
              cscomp-current-flavor cflavor)

        ;; now collect completion options depending on the "flavor" of
        ;; completion request:
        ;;
        ;; type - completing on m/f/p for a type, possibly with a fragment.
        ;;   p1 holds the typename from the buffer, could be partially
        ;;   qualified, in other words it might include a namespace.
        ;;   p1-type holds the fully qualified type name. p2 holds the
        ;;   fragment that will be completed.
        ;;
        ;; namespace - completing on a namespace.  The result can be a child
        ;;   namespace or a type within a namespace.  When this is the case,
        ;;   p1 holds the qualifying namespace if any, and p2 holds the type
        ;;   or ns name fragment to complete on.
        ;;
        ;; ctors - completing on type constructors.  p1 holds any qualifying
        ;;   ns, and p2 holds the typename (or ctor name, which is the same
        ;;   thing).
        ;;
        ;; this - completing on a m/f/p of the local instance
        ;;
        ;; local - completing on a m/f/p of local variables or method args
        ;;

        ;; Reset the completion list - the thing that is used
        ;; when the "no menu" version of complete-at-pont is used.
        (setq cscomp-current-list nil)

        (cscomp-log 2 "find-completion-for-split, flav(%s)" cflavor)

        (cond

         ((string= cflavor "type")
          (let* ((type-info (cscomp-get-typeinfo p1-type))
                 (full-list (cscomp-completion-list-for-type type-info)))
            (cond

             (is-func ;; it's a specific function on a type. Get the list of overloads.
              (setq cscomp-current-list
                    (cscomp-find-all-type-completions (substring p2 0 -1) full-list is-static))
              (cscomp-log 3 "find-completion-for-split: [%s]"
                          (prin1-to-string cscomp-current-list)))
             (t ;; could be anything: method, prop, field. Build the list.
              (setq cscomp-current-list
                    (cscomp-find-all-type-completions p2 full-list is-static))

              (cscomp-log 3 "find-completion-for-split: [%s]"
                          (prin1-to-string cscomp-current-list))))))


         ((string= cflavor "namespace")
          (cscomp-log 2 "find-completion-for-split is-func(%s) bot(%d)"
                      is-func beginning-of-token1)

          ;;(if is-ctor
              ;; provide type names, or child namespaces
              (let ((namespaces-to-scan
                     (if (null p1)
                         ;; search for completions in any of the implicit
                         ;; namespaces: any imported via using clauses, plus
                         ;; the ambient namespace.
                         ;;
                         ;; TODO: add types and child ns for the ambient
                         ;; namespace.
                         (cscomp-referenced-assemblies)
                       ;; else, search only in the explicitly
                       ;; provided ns.
                       (list p1))))

                (loop
                 for x in namespaces-to-scan do
                 (progn
                   (cscomp-load-one-assembly x)
                   (let* ((nsinfo
                           (cscomp-get-completions-for-namespace x))
                          (full-list
                           (cscomp-completion-list-for-ns nsinfo)))

                     (setq cscomp-current-list
                           (append cscomp-current-list
                                   (cscomp-find-all-ns-completions p2 full-list))))))

                (cscomp-log 3 "ctors: [%s]"
                            (prin1-to-string cscomp-current-list)))

              ;;(error "find-completion-for-split: bad state"))
            )


         ((string= cflavor "ctors")
          (let* ((short-name (substring p2 0 -1))
                 (type-name (if (stringp p1)
                                (concat p1 "." short-name)
                              (cscomp-qualify-type-name short-name)))
                 (ctor-info (cscomp-get-type-ctors type-name))
                 (full-list (cscomp-completion-list-for-ctors ctor-info)))

            ;; present all constructors
            (setq cscomp-current-list full-list)))


         ;; (cond
         ;;  (is-func ;; it's a function on a type - see if a constructor
         ;;   (cond
         ;;    ((save-excursion
         ;;       (goto-char beginning-of-token1)
         ;;       (re-search-backward "\\<new[ \t\n\f\v]+" nil t))
         ;;     ;; it's a constructor
         ;;     (let* ((type-name (concat p1 "." (substring p2 0 -1)))
         ;;            (ctor-info (cscomp-get-type-ctors type-name))
         ;;            (full-list (cscomp-completion-list-for-ctors ctor-info)))
         ;;
         ;;       ;; present all constructors
         ;;       (setq cscomp-current-list full-list)))
         ;;
         ;;    (t
         ;;     ;; not a constructor.  Must be some other function.
         ;;     (error "completion on static functions is not (yet?) supported."))))
         ;;
         ;;  (t

         ;; $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

         ((string= cflavor "local")
          (let ((instance-vars (cscomp-matching-instance-vars p2))
                (locals (cscomp-matching-local-vars p2)))
            (setq cscomp-current-list
                  (nconc instance-vars locals))))

         ((string= cflavor "this")
          (setq cscomp-current-list
                (cscomp-matching-instance-members p2)))

         ((string= cflavor "mixed")
          (setq r (cscomp-get-all-known-matches p2))
          (if (listp r)
              (setq cscomp-current-list
                    (mapcar '(lambda (listitem)
                               (list
                                ;; the thing to return when this menu item is selected
                                (cond
                                 ((string= "type" (car listitem))
                                  (if (and p1 (string-match (concat "^" p1)
                                                            (cadr listitem)))
                                      (cadr listitem)
                                    (string-match (concat "\\.\\(" p2 ".*\\)") (cadr listitem))
                                    (match-string 1 (cadr listitem))))

                                 (t
                                  ;;(string-match (concat "\\.\\(" p2 ".*\\)") (cadr listitem))
                                  (string-match (concat "^\\(" p2 ".*\\)") (cadr listitem))
                                  (match-string 1 (cadr listitem))))

                                ;; the description that will be visible in the menu
                                ;; for this menu item.
                                (if (eq (length listitem) 3)
                                    (nth 2 listitem)
                                  (concat (cadr listitem) " | (" (car listitem) ")"))))
                            r))

            (error "Cannot complete.")))

         ((string= cflavor "nothing")
          (message "Nothing to complete."))

         (t
          (cscomp-log 1 "find-completion-for-split, unknown flavor (%s)"
                      cflavor)
          (error "Cannot complete.")))))))





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

The incoming CLIST-ITEM is one item from the completion list; it
is either a (NAME DESCRIP) pair, or a simple NAME.

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
something suitable for use within a menu.

"
  (if (consp clist-item)
      (let ((mpfv-name (car clist-item)) ;; name of method, prop, field, variable
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
                                    (if (string-match ".+\\." mpfv-name)
                                        (substring mpfv-name
                                                        (match-end 0))
                                      mpfv-name)
                                  mpfv-name)))

            (cons (concat mpfv-name " | " descrip)
                  (if (fboundp 'yas/expand-snippet)
                      (cscomp-yasnippet-for-arglist name-to-insert arglist)
                    (concat name-to-insert arglist)))))

         (t
          ;; The completion is not a method - just
          ;; insert the name of the field/prop as a string.
          (cons (concat mpfv-name " | " descrip)
                mpfv-name))))

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
  ;; For a namespace, the clist is a list of strings, names
  ;; of types or child namespaces. (I think this is still true)
  ;;
  ;; In each case we want to sort the list alphabetically.
  ;;

  (let* ((menu-map (cons "ignored"
                         (mapcar 'cscomp-get-menu-item-for-clist-item
                                 cscomp-current-list)))
         (menu-result (cscomp--popup-menu
                       (list (or title "Completions...") menu-map))))

    (cscomp-log 1 "menu-result: %s" (prin1-to-string menu-result))

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
          ;; set mouse position
          (set-mouse-position (cscomp-selected-frame) (current-column) (cscomp-current-row))
          ;; inquire mouse position
          (setq pixel-pos (mouse-pixel-position))
          ;; restore mouse position
          (set-mouse-position (car mouse-pos) (car (cdr mouse-pos)) (cdr (cdr mouse-pos)))

          (setq ret (list (+ (cadr pixel-pos) 40) (cdr (cdr pixel-pos)))))
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


(defun cscomp--popup-menu (menu-data)
  "Pop up the completion menu at point, using the data MENU-DATA.
MENU-DATA is a list of error and warning messages returned by
`cscomp-make-err-menu-data'."
  (if (featurep 'xemacs)
      (let* ((pos          (cscomp-get-point-pixel-pos))
             (x-pos        (nth 0 pos))
             (y-pos        (nth 1 pos))
             (event-props  '(button 1 x 1 y 1)))
        (setq event-props (plist-put event-props 'x x-pos))
        (setq event-props (plist-put event-props 'y y-pos))
        (popup-menu (cscomp-make-xemacs-menu menu-data)
                    (make-event 'button-press event-props)))
    (x-popup-menu (if (eval-when-compile (fboundp 'posn-at-point))
                      (cscomp-posn-at-point-as-event nil nil 42 20)
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
    (incf cscomp-current-list-index)
    (cscomp-log 3 "list index: %d" cscomp-current-list-index)
    (if (>= cscomp-current-list-index (length cscomp-current-list))
        (setq cscomp-current-list-index 0)) ;; rollover
    (setq elem (nth cscomp-current-list-index cscomp-current-list))

    (cscomp-log 3 "complete-cycle-candidates: looking at %s" (prin1-to-string elem))

    (cond

     ((listp elem)
      (let ((thing-to-insert (car elem)))
        (if thing-to-insert
            (progn
              (delete-region cscomp-current-beginning cscomp-current-end)
              (insert thing-to-insert)
              (set-marker cscomp-current-end
                          (+ (marker-position
                              cscomp-current-beginning) (length thing-to-insert)))
              ;; display the description of the completioni n the minibuffer
              (message "cscomp: %s" (cadr elem)))

        (cscomp-log 1 "No completion at this point! (cycle)"))))

     ;; elem is an atom
     (t
      (let ((thing-to-insert elem))
        (delete-region cscomp-current-beginning cscomp-current-end)
        (insert thing-to-insert)
        (set-marker cscomp-current-end
                    (+ (marker-position cscomp-current-beginning) (length thing-to-insert) ))
        ;; display the description of the completioni n the minibuffer
        (message "cscomp: %s" elem))))))



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
              (cscomp-find-completion-for-split split (car parse-result))
              (setq cscomp-current-list-index -1)
              (cscomp-cycle-candidates)))))))



(defun cscomp-complete-at-point-menu ()
  "Pops up a menu with a list of completions for point.  When completing
on a type or instance, the menu items are the field, property and method
names; when completing on a namespace, the menu items are child namespaces
or types.

When the user makes a selection from the menu, the selected item
is inserted at point. In the case of a method with parameters,
and when yasnippet.el is available (see
http://code.google.com/p/yasnippet/), the inserted thing is a
parameterized snippet, a template that can then be filled in by
the user.  If yasnippet is not available, then the user's menu
choice is just inserted into the buffer as static text.

See `cscomp-complete-at-point' for an alternative to this function that
lets you cycle through the potential completions at point, with each
completion appearing in the buffer in turn.

"
  (interactive)
  (let* ((parse-result (cscomp-parse-csharp-expression-before-point))
         (split (and parse-result (cadr parse-result))))

    (if (not (null split))
        (progn
          (cscomp-find-completion-for-split split (car parse-result))

          (if cscomp-current-list
              (let* ((title (if (car split)
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




;; ========================================================================
;; integration with Matsuyama's auto-complete.el

(defun cscomp-completions-at-point ()
  "Generates completions of field, prop or method names if the
thing being completed is a type or instance, or on namespace
members - child namespaces or types - if the thing being
completed is a namespace.

The result is a list of strings.

This fn is used by Matsuyama's auto-complete.el, when using
csharp-completion as a source.

See also `cscomp-complete-at-point-menu' and `cscomp-complete-at-point'
for interactive functions that do, just about the same thing.
"
  (progn
    (let* ((parse-result (cscomp-parse-csharp-expression-before-point))
           (split (and parse-result (cadr parse-result))))

      (if (not (null split))
          (progn
            (cscomp-find-completion-for-split split (car parse-result) t)

            (mapcar
             ;; I thought I needed a separate lambda when the p1
             ;; was non-nil, but maybe that is not the case.
             (if cscomp-current-p1
                 '(lambda (item)
                    ;;(cscomp-log 3 "considering item: %s" item)
                    (let ((result
                           (car item)
                           ;;(concat cscomp-current-p1 "." (car item))
                           ))
                      ;;(cscomp-log 3 "result: %s" result)
                      result))

               '(lambda (item)
                  ;;(cscomp-log 3 "considering item: %s" item)
                  (car item)))

             cscomp-current-list))
        nil))))


;; (eval-after-load "auto-complete"
;;   '(progn
;;      (if (fboundp 'ac-candidates-1)
;;          (progn
;;            ;; In lieu of using the ac-define-source macro,
;;            ;; define the function and the var separately.
;;            (defvar ac-source-csharp
;;              '((init . (setq cscomp-current-list nil))
;;                (candidates . cscomp-completions-at-point)
;;                (cache)))
;;
;;            (defun ac-complete-csharp ()
;;              (interactive)
;;              (auto-complete '(ac-source-csharp)))))))



;; In lieu of using the ac-define-source macro,
;; define the function and the var separately.
(defvar ac-source-csharp
  '((init . (setq cscomp-current-list nil))
    (candidates . cscomp-completions-at-point)
    (cache)))


(defun ac-complete-csharp ()
  "performs auto-completion using the source `ac-source-csharp'."
  (interactive)
  (auto-complete '(ac-source-csharp)))


(provide 'csharp-completion)

;;; end of csharp-completion.el
