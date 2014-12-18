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
  (let* ((buffer (find-file-read-only "d:/Throwaway/test.cs")))
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


;;(ert-run-tests-interactively t)
