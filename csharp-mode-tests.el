(require 'ert)
(require 'cl-lib)
(require 'csharp-mode)
(require 'cl)
(require 'package)

;; development only packages, not declared as a package-dependency
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(dolist (p '(assess))
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

;;; test-helper functions

(defmacro assess-face-in-text= (testee &rest assessments)
  (when assessments
    (let* ((text (car assessments))
           (face (cadr assessments))
           (rest (cddr assessments)))
      `(progn
         (require 'assess)
         (should (assess-face-at= ,testee 'csharp-mode ,text ,face))
         (assess-face-in-text= ,testee ,@rest)))))

(defmacro assess-face-in-file= (file-name &rest assessments)
  (let* ((find-file-hook nil) ;; disable vc-mode hooks
         (buffer (find-file-read-only file-name))
         (contents (buffer-substring-no-properties (point-min) (point-max))))
    (kill-buffer buffer)
    `(assess-face-in-text= ,contents ,@assessments)))

;;; actual tests

(ert-deftest activating-mode-doesnt-cause-failure ()
  (with-temp-buffer
    (csharp-mode)
    (should
     (equal 'csharp-mode major-mode))))

(defvar debug-res nil)

(ert-deftest fontification-of-literals-detects-end-of-strings ()
  (assess-face-in-file= "./test-files/fontification-test.cs"
                        "bool1"      'font-lock-type-face
                        "Reference1" 'font-lock-variable-name-face
                        "false"      'font-lock-constant-face
                        "bool2"      'font-lock-type-face
                        "Reference2" 'font-lock-variable-name-face
                        "true"       'font-lock-constant-face
                        ))

(ert-deftest fontification-of-compiler-directives ()
  ;; this replaces the manual test of
  ;; test-files/fontification-test-compiler-directives.cs, but file
  ;; has been kept around to assist manual testing/verification.
  (assess-face-in-text=
   "#region test\nbool bar = true;"
   ;; should not be interpreted as string because of trailing \!
   "bool" 'font-lock-type-face
   "bar"  'font-lock-variable-name-face
   "true" 'font-lock-constant-face
   )
  (should (assess-face-at=
           "#region test'\nx = true;"
           'csharp-mode
           ;; should not be interpreted as string because of trailing \!
           "true" 'font-lock-constant-face
           ))
  (should (assess-face-at=
           "#region test\"\nx = true;"
           'csharp-mode
           ;; should not be interpreted as string because of trailing \!
           "true" 'font-lock-constant-face
           )))

(ert-deftest fontification-of-compiler-directives-after-comments ()
  ;; this replaces the manual test of
  ;; test-files/fontification-test-compiler-directives-with-comments.cs, but file
  ;; has been kept around to assist manual testing/verification.
  (assess-face-in-file= "./test-files/fontification-test-compiler-directives-with-comments.cs"
                        "case1" 'font-lock-comment-face
                        "case2" 'font-lock-comment-face))

(defun list-repeat-once (mylist)
  (append mylist mylist))

(ert-deftest build-warnings-and-errors-are-parsed ()
  (dolist (test-case
           `(("./test-files/msbuild-warning.txt" ,csharp-compilation-re-msbuild-warning
              ,(list-repeat-once
                '("Class1.cs"
                  "Folder\\Class1.cs"
                  "Program.cs"
                  "Program.cs")))
             ("./test-files/msbuild-error.txt" ,csharp-compilation-re-msbuild-error
              ,(list-repeat-once
                '("Folder\\Class1.cs")))
             ("./test-files/msbuild-concurrent-warning.txt" ,csharp-compilation-re-msbuild-warning
              ,(list-repeat-once
                '("Program.cs")))
             ("./test-files/msbuild-concurrent-error.txt" ,csharp-compilation-re-msbuild-error
              ,(list-repeat-once
                '("Program.cs")))
             ("./test-files/msbuild-square-brackets.txt" ,csharp-compilation-re-msbuild-error
              ,(list-repeat-once
                '("Properties\\AssemblyInfo.cs"
                  "Program.cs"
                  "Program.cs")))
             ("./test-files/msbuild-square-brackets.txt" ,csharp-compilation-re-msbuild-warning
              ,(list-repeat-once
                '("Program.cs")))
             ("./test-files/xbuild-warning.txt" ,csharp-compilation-re-xbuild-warning
              ,(list-repeat-once
                '("/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ClassLibrary1/Class1.cs"
                  "/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ClassLibrary1/Folder/Class1.cs"
                  "/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ConsoleApplication1/Program.cs"
                  "/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ConsoleApplication1/Program.cs"
                  "/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ConsoleApplication1/Program.cs")))
             ("./test-files/xbuild-error.txt" ,csharp-compilation-re-xbuild-error
              ,(list-repeat-once
                '("/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ClassLibrary1/Folder/Class1.cs")))
             ("./test-files/devenv-error.txt" ,csharp-compilation-re-xbuild-error
              ("c:\\working_chad\\dev_grep\\build_grep_database\\databaseconnection.cpp"
               "c:\\working_chad\\dev_grep\\build_grep_database\\databaseconnection.cpp"
               "c:\\working_chad\\dev_grep\\build_grep_database\\databaseconnection.cpp"))
             ("./test-files/devenv-error.txt" ,csharp-compilation-re-xbuild-warning
              ("c:\\working_chad\\dev_grep\\build_grep_database\\databaseconnection.cpp"))
             ("./test-files/devenv-mixed-error.txt" ,csharp-compilation-re-xbuild-error
              ("C:\\inservice\\SystemTesting\\OperateDeviceProxy\\OperateDevice_Proxy\\Program.cs"
               "C:\\inservice\\SystemTesting\\OperateDeviceProxy\\OperateDevice_Proxy\\Program.cs"
               "C:\\inservice\\SystemTesting\\OperateDeviceProxy\\OperateDevice_Proxy\\Program.cs"
               "c:\\inservice\\systemtesting\\operationsproxy\\operationsproxy.cpp"
               "c:\\inservice\\systemtesting\\operationsproxy\\operationsproxy.cpp"
               "c:\\inservice\\systemtesting\\operationsproxy\\operationsproxy.cpp"))))

    (let* ((file-name (car test-case))
           (regexp    (cadr test-case))
           (matched-file-names (cl-caddr test-case))
           (times     (length matched-file-names))
           (find-file-hook '()) ;; avoid vc-mode file-hooks when opening!
           (buffer (find-file-read-only file-name)))
      ;; (message (concat "Testing compilation-log: " file-name))
      (dotimes (number times)
        (let* ((expected (nth number matched-file-names)))
          ;; (message (concat "- Expecting match: " expected))
          (re-search-forward regexp)
          (should
           (equal expected (match-string 1)))))
      (kill-buffer buffer))))

(defmacro def-imenutest (testname filename index &rest body)
  `(ert-deftest ,testname ()
     (let* ((find-file-hook nil) ;; avoid vc-mode file-hooks when opening!
            (buffer         (find-file-read-only ,filename))
            (,index         (csharp--imenu-create-index-function)))
       ,@body
       (kill-buffer buffer))))

(defun imenu-get-item (index haystack)
  (setq result nil)
  (dolist (item index)
    (when (not result)
      (let ((name (car item))
            (value (cdr item)))
        (if (string-prefix-p haystack name)
            (setq result item)
          (when (listp value)
              (setq result (imenu-get-item value haystack)))))))
  result)

(def-imenutest imenu-parsing-supports-generic-parameters
  "./test-files/imenu-generics-test.cs" imenu-index
  (dolist (item '("NoGeneric(" "OneGeneric<T>(" "TwoGeneric<T1,T2>("))
    (should (imenu-get-item imenu-index (concat "(method) " item)))))

(def-imenutest imenu-parsing-supports-comments
  "./test-files/imenu-comment-test.cs" imenu-index
  (dolist (item '("HasNoComment(" "HasComment(" "CommentedToo("))
    (should (imenu-get-item imenu-index (concat "(method) " item)))))

(def-imenutest imenu-parsing-supports-explicit-interface-properties
  "./test-files/imenu-interface-property-test.cs" imenu-index
  (should (imenu-get-item imenu-index "(prop) IImenuTest.InterfaceString")))

(def-imenutest imenu-parsing-supports-explicit-interface-methods
  "./test-files/imenu-interface-property-test.cs" imenu-index
  (should (imenu-get-item imenu-index "(method-inf) IImenuTest.MethodName")))

(def-imenutest imenu-parsing-provides-types-with-namespace-names
  "./test-files/imenu-namespace-test.cs" imenu-index
  (should (imenu-get-item imenu-index "class ImenuTest.ImenuTestClass"))
  (should (imenu-get-item imenu-index "interface ImenuTest.ImenuTestInterface"))
  (should (imenu-get-item imenu-index "enum ImenuTest.ImenuTestEnum")))

(defvar csharp-hook1 nil)
(defvar csharp-hook2 nil)

(ert-deftest activating-mode-triggers-all-hooks ()
  (add-hook 'csharp-mode-hook (lambda () (setq csharp-hook1 t)))
  (add-hook 'prog-mode-hook   (lambda () (setq csharp-hook2 t)))

  (with-temp-buffer
    (csharp-mode)
    (should (equal t (and csharp-hook1
                          csharp-hook2)))))

(defvar c-mode-hook-run nil)
(ert-deftest avoid-runing-c-mode-hook ()
  (add-hook 'c-mode-hook (lambda () (setq c-mode-hook-run t)))

  (with-temp-buffer
    (csharp-mode)
    (should-not c-mode-hook-run)))

(ert-deftest indentation-rules-should-be-as-specified-in-test-doc ()
  (let* ((buffer (find-file "test-files/indentation-tests.cs"))
         (orig-content)
         (indented-content))
    ;; double-ensure mode is active
    (csharp-mode)

    (setq orig-content (buffer-substring-no-properties (point-min) (point-max)))
    (indent-region (point-min) (point-max))
    (setq indented-content (buffer-substring-no-properties (point-min) (point-max)))

    (should (equal orig-content indented-content))))

;;(ert-run-tests-interactively t)
