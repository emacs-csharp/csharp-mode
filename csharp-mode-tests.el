(require 'ert)
(require 'csharp-mode)

;;; test-helper functions

(defun move-to-line-after (text)
  (search-forward text)
  (move-beginning-of-line 1)
  (forward-line 1))

(defun get-current-line-contents ()
  (let* ((start)
         (end))
    (move-beginning-of-line 1)
    (setq start (point))
    (move-end-of-line 1)
    (setq end (point))
    (buffer-substring start end)))

;;; actual tests

(ert-deftest activating-mode-doesnt-cause-failure ()
  (with-temp-buffer
    (csharp-mode)
    (should
     (equal 'csharp-mode major-mode))))

(setq debug-res nil)

(ert-deftest fontification-of-literals-detects-end-of-strings ()
  ;; this test needs a double which also writes and generates the actual
  ;; test-content itself by inserting into a new temp buffer.
  (let* ((buffer (find-file-read-only "test-files/fontification-test.cs")))
    ;; double-ensure mode is active
    (csharp-mode)
    (goto-char (point-min))
    (let* ((buffer1)
           (buffer2))
      ;; get reference string
      (move-to-line-after "Literal1")
      (setq buffer1 (get-current-line-contents))

      ;; get verification string
      (move-to-line-after "Literal2")
      (setq buffer2 (get-current-line-contents))

      ;; check equality
      (setq debug-res (list buffer1 buffer2))
      (should
       (equal buffer1 buffer2)))))

(defun list-repeat-once (mylist)
  (append mylist mylist))

(ert-deftest build-warnings-and-errors-are-parsed ()
  (dolist (test-case
	   `(("./test-files/msbuild-warning.txt" ,csharp-compilation-re-msbuild-warning 8
              ,(list-repeat-once
                '("Class1.cs"
                  "Folder\\Class1.cs"
                  "Program.cs"
                  "Program.cs")))
	     ("./test-files/msbuild-error.txt" ,csharp-compilation-re-msbuild-error 2
              ,(list-repeat-once
                '("Folder\\Class1.cs")))
             ("./test-files/msbuild-concurrent-warning.txt" ,csharp-compilation-re-msbuild-warning 2
              ,(list-repeat-once
                '("Program.cs")))
             ("./test-files/msbuild-concurrent-error.txt" ,csharp-compilation-re-msbuild-error 2
              ,(list-repeat-once
                '("Program.cs")))
	     ("./test-files/xbuild-warning.txt" ,csharp-compilation-re-xbuild-warning 10
              ,(list-repeat-once
                '("/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ClassLibrary1/Class1.cs"
                  "/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ClassLibrary1/Folder/Class1.cs"
                  "/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ConsoleApplication1/Program.cs"
                  "/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ConsoleApplication1/Program.cs"
                  "/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ConsoleApplication1/Program.cs")))
	     ("./test-files/xbuild-error.txt" ,csharp-compilation-re-xbuild-error 2
              ,(list-repeat-once
                '("/Users/jesseblack/Dropbox/barfapp/ConsoleApplication1/ClassLibrary1/Folder/Class1.cs")))
	     ))

    (let* ((file-name (car test-case))
	   (regexp    (cadr test-case))
	   (times     (caddr test-case))
           (matched-file-names (cadddr test-case))
	   (find-file-hook '()) ;; avoid vc-mode file-hooks when opening!
	   (buffer (find-file-read-only file-name)))
      (dotimes (number times)
	(re-search-forward regexp)
        (should
         (equal (nth number matched-file-names) (match-string 1))))
      (kill-buffer buffer))))

;;(ert-run-tests-interactively t)
