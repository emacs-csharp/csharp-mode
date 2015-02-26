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

(ert-deftest build-warnings-and-errors-are-parsed ()
  (dolist (test-case
	   `(("./test-files/msbuild-warning.txt" ,csharp-compilation-re-msbuild-warning 3)
	     ("./test-files/msbuild-error.txt" ,csharp-compilation-re-msbuild-error 1)
	     ("./test-files/xbuild-warning.txt" ,csharp-compilation-re-xbuild-warning 5)
	     ("./test-files/xbuild-error.txt" ,csharp-compilation-re-xbuild-error 1)
	     ))

    (let* ((file-name (car test-case))
	   (regexp    (cadr test-case))
	   (times     (caddr test-case))
	   (find-file-hook '()) ;; avoid vc-mode file-hooks when opening!
	   (buffer (find-file-read-only file-name)))
      (dotimes (number times)
	(re-search-forward regexp))
      (kill-buffer buffer))))

;;(ert-run-tests-interactively t)
