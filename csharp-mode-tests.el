;;; csharp-mode-tests.el --- Tests for csharp-mode.el            -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

;; Author: Josten Kjønigsen <jostein@gmail.com>
;; Keywords: tests

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

(require 'ert)
(require 'cl-lib)
(require 'csharp-mode)
(require 'package)

;; development only packages, not declared as a package-dependency
;; FIXME: loading a .el file from `load-path' should not change user's settings
;; like that.  It can happen without the user explicitly requesting it!
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))

;; required to resolve SEQ (or anything on elpa) on Emacs25.
(setq package-check-signature nil)

;; assess depends on dash 2.12.1, which is no longer available
;; installing dash, resolves 2.13.0, and fixes this broken dependency.
(dolist (p '(dash assess))
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

(when (and (>= emacs-major-version 29)
           (string-lessp "5.35.0" c-version))
  (ert-deftest fontification-of-multiline-strings ()
    (assess-face-in-file= "./test-files/multiline-strings.cs"
                          "Literal0" 'font-lock-variable-name-face
                          "Literal1" 'font-lock-variable-name-face
                          "Literal2" 'font-lock-variable-name-face
                          "Literal3" 'font-lock-variable-name-face
                          "Literal4" 'font-lock-variable-name-face
                          "Literal5" 'font-lock-variable-name-face
                          "Literal6" 'font-lock-variable-name-face
                          "Literal7" 'font-lock-variable-name-face)))

(ert-deftest fontification-of-constants ()
  (require 'assess)
  (assess-face-in-text=
   "testBool1 = true;\ntestBool2 = false;\ntestObj = null;\ntestProp = value;"
   "false" 'font-lock-constant-face
   "true"  'font-lock-constant-face
   "null"  'font-lock-constant-face
   "value" 'font-lock-constant-face
   ))

(ert-deftest fontification-of-package ()
  (require 'assess)
  (assess-face-in-text=
   "var package = true;"
   "package" 'font-lock-variable-name-face))

(ert-deftest fontification-of-functions ()
  (require 'assess)
  (assess-face-in-text= "var foo = bar.Baz()"
                        "Baz" 'font-lock-function-name-face)
  (assess-face-in-text= "var foo = bar.Baz<Quux>()"
                        "Baz" 'font-lock-function-name-face
                        "Quux" 'font-lock-type-face))

(ert-deftest fontification-of-import ()
  (require 'assess)
  (assess-face-in-text=
   "var import = true;"
   "import" 'font-lock-variable-name-face))

(ert-deftest fontification-of-literals-allows-multi-line-strings ()
  (require 'assess)
  (should (assess-face-at=
           "string Literal = \"multi-line\nstring\";"
           'csharp-mode
           ;; should be interpreted as error
           18 'font-lock-warning-face
           ))
  (should (assess-face-at=
           "string Literal = @\"multi-line\nstring\";"
           'csharp-mode
           ;; should not be interpreted as error because of @
           19 'font-lock-string-face
           )))

;; (ert-deftest fontification-of-compiler-directives ()
;;   ;; this replaces the manual test of
;;   ;; test-files/fontification-test-compiler-directives.cs, but file
;;   ;; has been kept around to assist manual testing/verification.
;;   (assess-face-in-file= "test-files/fontification-test-compiler-directives.cs"
;;                         "strReference" 'font-lock-string-face
;;                         "strVerification" 'font-lock-string-face
;;                         "singleQuote" 'font-lock-string-face
;;                         "doubleQuote" 'font-lock-string-face)

;;   (assess-face-in-text=
;;    "#region test\nbool bar = true;"
;;    ;; should not be interpreted as string because of trailing \!
;;    "bool" 'font-lock-type-face
;;    "bar"  'font-lock-variable-name-face
;;    "true" 'font-lock-constant-face
;;    )
;;   (should (assess-face-at=
;;            "#region test'\nx = true;"
;;            'csharp-mode
;;            ;; should not be interpreted as string because of trailing \!
;;            "true" 'font-lock-constant-face
;;            ))
;;   (should (assess-face-at=
;;            "#region test\"\nx = true;"
;;            'csharp-mode
;;            ;; should not be interpreted as string because of trailing \!
;;            "true" 'font-lock-constant-face
;;            )))

(ert-deftest fontification-of-compiler-directives-after-comments ()
  (assess-face-in-file= "./test-files/fontification-test-compiler-directives-with-comments.cs"
                        "case1" 'font-lock-comment-face
                        "case2" 'font-lock-comment-face))

(ert-deftest fontification-of-method-names ()
  (assess-face-in-file= "./test-files/imenu-method-test.cs"
                        "OpenWebServicesAsync" 'font-lock-function-name-face
                        "ToString"             'font-lock-function-name-face
                        "Equals"               'font-lock-function-name-face
                        "AbstractMethod"       'font-lock-function-name-face
                        "UnsafeCopy"           'font-lock-function-name-face
                        ;; "GenericMethod1"       'font-lock-function-name-face
                        ;; "GenericMethod2"       'font-lock-function-name-face
                        ))

(ert-deftest fontification-of-using-statements ()
  (assess-face-in-file= "./test-files/using-fontification.cs"
                        "using" 'font-lock-keyword-face
                        "Reference" 'font-lock-variable-name-face
                        "Under_scored" 'font-lock-variable-name-face
                        "WithNumbers09" 'font-lock-variable-name-face
                        "Ok" 'font-lock-variable-name-face
                        "WithNumbers09" 'font-lock-variable-name-face
                        "OkV2" 'font-lock-variable-name-face
                        ))

(ert-deftest fontification-of-namespace-statements ()
  (assess-face-in-file= "./test-files/namespace-fontification.cs"
                        "namespace" 'font-lock-keyword-face
                        "Reference" 'font-lock-variable-name-face
                        "Under_scored" 'font-lock-variable-name-face
                        "WithNumbers09" 'font-lock-variable-name-face
                        "Ok" 'font-lock-variable-name-face
                        "WithNumbers09" 'font-lock-variable-name-face
                        "Ok" 'font-lock-variable-name-face
                        ))

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
               "c:\\inservice\\systemtesting\\operationsproxy\\operationsproxy.cpp"))

             ("./test-files/dotnet-nuget-error.txt" ,csharp-compilation-re-dotnet-error
              ("/home/jostein/build/sample-app/sample-app.csproj"))
             ("./test-files/dotnet-nuget-warning.txt" ,csharp-compilation-re-dotnet-warning
              ("/home/jostein/build/sample-app/sample-app.csproj"))
             ("./test-files/dotnet-test-fail-xunit.txt" ,csharp-compilation-re-dotnet-testfail
              ("/home/jostein/build/sample-app/Module/Testee.cs"))))

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

(ert-deftest region-directive-comment-movement ()
  (find-file "test-files/region-fontification.cs")
  (csharp-mode)
  (goto-char (point-min))
  (search-forward "#region ")
  (forward-word 1)
  (forward-word -1)
  (should (looking-at "fontifies")))

;; (ert-deftest fontification-of-regions ()
;;   (require 'assess)
;;   (require 'm-buffer)
;;   (find-file "test-files/region-fontification.cs")
;;   (csharp-mode)
;;   (let ((buf (current-buffer)))
;;     ;; look for 'a region comment' - should always be a comment
;;     (should (assess-face-at= buf 'csharp-mode (lambda (buf) (m-buffer-match buf "a region comment")) 'font-lock-comment-face))
;;     ;; look for 'string' - should always be a type
;;     (should (assess-face-at= buf 'csharp-mode (lambda (buf) (m-buffer-match buf "string")) 'font-lock-type-face))))

(ert-deftest activating-mode-doesnt-clobber-global-adaptive-fill-regexp ()
  (let ((before adaptive-fill-regexp))
    (with-temp-buffer
      (csharp-mode))
    (should
     (equal before adaptive-fill-regexp))))

(ert-deftest activating-mode-style-defaults-to-csharp ()
  (with-temp-buffer
    (csharp-mode)
    (should
     (equal "csharp" c-indentation-style)))

  (let ((c-default-style "csharp"))
    (with-temp-buffer
      (csharp-mode)
      (should
       (equal "csharp" c-indentation-style))))

  (let ((c-default-style '((csharp-mode . "csharp")
                           (java-mode . "java"))))
    (with-temp-buffer
      (csharp-mode)
      (should
       (equal "csharp" c-indentation-style)))))

(ert-deftest inside-bracelist-test ()
  (with-temp-buffer
    (csharp-mode)
    (insert "public class A { public void F() {")
    (call-interactively #'newline)))

;;(ert-run-tests-interactively t)
;; (local-set-key (kbd "<f6>") '(lambda ()
;;                               (interactive)
;;                               (ert-run-tests-interactively t)))
