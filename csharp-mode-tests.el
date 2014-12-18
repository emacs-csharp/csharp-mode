
(require 'ert)
(require 'csharp-mode)

(ert-deftest activating-mode-doesnt-cause-failure ()
  (with-temp-buffer
    (csharp-mode)
    (should
     (equal 'csharp-mode major-mode))))

;;(ert-run-tests-interactively t)

