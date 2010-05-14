;;; aspx-mode.el --- mode for editing ASPX files
;;
;; Copyright (C) 2010  Dino Chiesa
;;
;; Author:   Dino Chiesa <dpchiesa@hotmail.com>
;; Created:  May 2010
;; Version:  0.1
;; Keywords: C#, languages, extensions, files
;; URL:      ???
;;
;; This is an emacs mode for editing single-file ASPX modules,
;; which can contain HTML, javascript, C# or VB, and
;; CSS code.
;;
;; It relies on multi-mode.
;; (see http://www.loveshack.ukfsn.org/emacs )
;;
;; It provides context-sensitive fontification and indent in all of the
;; various chunks o an ASPX file. Browser-side Javascript within script
;; blocks gets fontified correctly. Server-side script blocks get
;; fontified properly. CSS gets indented and fontified. And of course
;; HTML.
;;
;; It relies on espresso for the javascript mode, css-mode for CSS,
;; html-mode for HTML, and csharp-mode 0.7.5 or later for the
;; server-side c# code.
;;
;; Bugs:
;;
;;  - The fontification sort of turns on and off as you cursor through
;;    the buffer, making for a somewhat odd user experience.
;;
;;  - the fontification sometimes doesn't happen within a chunk, til you
;;    modify the text within the chunk.  There's probably a fix for
;;    this.
;;
;;
;; Thu, 13 May 2010  23:44
;;
;; Licensed according to the Microsoft Public License (Ms-PL)
;;
;; This license governs use of the accompanying software. If you use
;; the software, you accept this license. If you do not accept the
;; license, do not use the software.
;;
;; 1. Definitions
;;
;; The terms "reproduce," "reproduction," "derivative
;; works," and "distribution" have the same meaning here as under
;; U.S. copyright law.
;;
;; A "contribution" is the original software, or any additions or
;; changes to the software.
;;
;; A "contributor" is any person that distributes its contribution
;; under this license.
;;
;; "Licensed patents" are a contributor's patent claims that read
;; directly on its contribution.
;;
;; 2. Grant of Rights
;;
;; (A) Copyright Grant- Subject to the terms of this license,
;; including the license conditions and limitations in section 3,
;; each contributor grants you a non-exclusive, worldwide,
;; royalty-free copyright license to reproduce its contribution,
;; prepare derivative works of its contribution, and distribute its
;; contribution or any derivative works that you create.
;;
;; (B) Patent Grant- Subject to the terms of this license, including
;; the license conditions and limitations in section 3, each
;; contributor grants you a non-exclusive, worldwide, royalty-free
;; license under its licensed patents to make, have made, use, sell,
;; offer for sale, import, and/or otherwise dispose of its
;; contribution in the software or derivative works of the
;; contribution in the software.
;;
;; 3. Conditions and Limitations
;;
;; (A) No Trademark License- This license does not grant you rights
;; to use any contributors' name, logo, or trademarks.
;;
;; (B) If you bring a patent claim against any contributor over
;; patents that you claim are infringed by the software, your patent
;; license from such contributor to the software ends automatically.
;;
;; (C) If you distribute any portion of the software, you must
;; retain all copyright, patent, trademark, and attribution notices
;; that are present in the software.
;;
;; (D) If you distribute any portion of the software in source code
;; form, you may do so only under this license by including a
;; complete copy of this license with your distribution. If you
;; distribute any portion of the software in compiled or object code
;; form, you may only do so under a license that complies with this
;; license.
;;
;; (E) The software is licensed "as-is." You bear the risk of using
;; it. The contributors give no express warranties, guarantees or
;; conditions. You may have additional consumer rights under your
;; local laws which this license cannot change. To the extent
;; permitted under your local laws, the contributors exclude the
;; implied warranties of merchantability, fitness for a particular
;; purpose and non-infringement.
;;
;;;


(require 'multi-mode)
(require 'csharp-mode)
(require 'espresso "espresso.el")
;;(require 'javascript-mode "javascript.el")
(require 'css-mode)



(defvar aspx-mode-log-level 0
  "The current log level for operatopms specific to aspx-mode.
0 = NONE, 1 = Info, 2 = VERBOSE, 3 = DEBUG, 4 = SHUTUP ALREADY. ")


(defun aspx-mode-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `aspx-mode-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (if (<= level aspx-mode-log-level)
      (let* ((msg (apply 'format text args)))
        (message "aspx-mode: %s" msg))
    t))


(defconst aspx-mode-server-lang-re
  "\\([Cc]#\\|[Vv][Bb]\\)"
  "Regex for matching on the ASPX langauge.")


(defconst aspx-mode-server-script-start-re
  (concat
   "<\\(script\\|SCRIPT\\)[ \t\n\r\v\f]+"
   "\\("
   "language=[\"']"
   aspx-mode-server-lang-re
   "[\"'][ \t\n\r\v\f]+runat=[\"']server[\"']"
   "\\|"
   "runat=[\"']server[\"'][ \t\n\r\v\f]+language=[\"']"
   aspx-mode-server-lang-re
   "[\"']"
   "\\)[ \t\n\r\v\f]*>"
   )
"Regex for matching the <script> tag that begins a block of
ASPX server-side code.  It tries to match on <script language='C#' runat='server'...>
as well as <script runat='server' language='C#' ...>
" )


(defconst aspx-mode-page-decl-re
  (concat
   "<%@"
   "[ \t\n\r\v\f]+"
   "\\(Page\\|Control\\)"
   "[ \t\n\r\v\f]+"
   )

  "Regex for matching the page/control declaration"
  )


(defconst aspx-mode-browser-script-start-re
  (concat
   "<\\(script\\|SCRIPT\\)[ \t\n\r\v\f]+"
   "\\("
   "type=[\"']text/javascript[\"'][ \t\n\r\v\f]+language=[\"'][Jj]ava[Ss]cript[\"']"
   "\\|"
   "language=[\"'][Jj]ava[Ss]cript[\"'][ \t\n\r\v\f]+type=[\"']text/javascript[\"']"
   "\\|"
   "type=[\"']text/javascript[\"']"
   "\\|"
   "language=[\"'][Jj]ava[Ss]cript[\"']"
   "\\)")

"Regex for matching the <script> tag that begins a block of
browser-side javascript code.  It tries to match on
<script language='javascript' ...>  or
<script type='text/javascript' ...>  or
<script type='text/javascript' language='javascript' ...> or
<script language='javascript' type='text/javascript' ...>
")


(defconst aspx-mode-css-block-start-re
  (concat
   "<\\(style\\|STYLE\\)"
   "[ \t\n\r\v\f]+"
   "type=[\"']text/css[\"']"
   "[ \t\n\r\v\f]*"
   ">")
  "Regex to match the beginning of a CSS block."
  )



(defvar aspx-mode--last-chunk-result nil
  "cached result of the last chunk analysis")

(defvar aspx-mode-update-interval 1
  "The amount of time in seconds after the last change before trying to
re-fontify the current block.")

(defvar aspx-mode-timer nil)





;; When in multi-mode, I want espresso to indent to the
;; <script> tag.  Use advice on the espresso indentation
;; calculation, to make that happen.
(defadvice espresso--proper-indentation (after
                                         aspx-mode-advice-1
                                         compile activate)
  (if (and (boundp 'multi-mode)
           multi-mode)
      (if (eq ad-return-value 0)
          (setq ad-return-value (+ ad-return-value 4)))))




;; =======================================================
;; dinoch - Thu, 13 May 2010  23:38
;; factored out so that I can attach advice to it.
;; Did this for multi-mode support in ASPX files.
;; =======================================================
(defun css--proper-indentation ()
  (save-excursion
    (back-to-indentation)
    (let ((p (parse-partial-sexp (point-min) (point)))
          (end-brace-p (looking-at "}")))
      (cond
       ((or (nth 8 p) (looking-at "/[/*]"))
        (current-indentation))
       ((save-excursion
          (and (skip-chars-backward " \t\n:,")
               (looking-at "[:,]")))
        (save-excursion
          (css-re-search-backward "^[ \t]*\\w")
          (+ (current-indentation) css-indent-level)))
       ((nth 1 p)
        (save-excursion
          (goto-char (nth 1 p))
          (+ (current-indentation) (if end-brace-p 0 css-indent-level))))
       (t
        0)))))


(defun css-indent-line ()
  (interactive)
  (let ((indent (css--proper-indentation))
        (offset (- (current-column) (current-indentation))))
    (indent-line-to indent)
    (if (> offset 0) (forward-char offset))))



;; Likewise with css-mode indentation.
(defadvice css--proper-indentation (after
                                    aspx-mode-advice-2
                                    compile activate)
  (if (and (boundp 'multi-mode)
           multi-mode)
      (if (eq ad-return-value 0)
          (setq ad-return-value (+ ad-return-value 4)))))






(defun aspx-mode-timer-elapsed ()
  (aspx-mode-log 2 "timer fired.")
  ;;(run-hooks 'aspx-mode-timer-elapsed-hook)
  (aspx-mode-refontify-current-chunk-after-idle))


(defun aspx-mode-restart-timer ()
  (if (timerp aspx-mode-timer) (cancel-timer aspx-mode-timer))
  (setq aspx-mode-timer
        (run-with-timer aspx-mode-update-interval nil 'aspx-mode-timer-elapsed)))

(defun aspx-mode-after-change-fn (begin end length)
  (aspx-mode-maybe-invalidate-cached-chunk begin end length)
  (if multi-mode (aspx-mode-restart-timer)))



(defun aspx-mode-maybe-invalidate-cached-chunk (begin end old-length)
  (let ((new-length (- end begin))
        (old-end (+ begin old-length)))

    ;; Invalidate if the length changed (we need to recalc the chunk limits)
    ;; or if the change traversed the end of the chunk.
    (if (and aspx-mode--last-chunk-result
             (or (/= old-length new-length)
                 (>= old-end  (nth 2 aspx-mode--last-chunk-result))))
        (setq aspx-mode--last-chunk-result nil))))



(defun aspx-mode-refontify-current-chunk-after-idle ()
  "Fontify the current (cached) chunk.  This fn is called after a timer
expires, when the buffer has sats idle for 2s.
"
  (aspx-mode-log 2 "fontifying (%d %d)"
           (nth 1 aspx-mode--last-chunk-result)
           (nth 2 aspx-mode--last-chunk-result))

  (if aspx-mode--last-chunk-result
      ;; Remove text props in the chunk, to force a new fontification
      ;; later.  Do this within a save-buffer-state, because we're not
      ;; *really* changing the buffer.
      (c-save-buffer-state  ()
        (set-text-properties (nth 1 aspx-mode--last-chunk-result)
                             (nth 2 aspx-mode--last-chunk-result)
                             nil))))






(defun aspx-mode-determine-current-chunk (pos)
  "Determine the type (mode) and limits of the chunk at POS.
Return (MODE START END), where MODE is one of `csharp-mode',
`javascript-mode', `html-mode', or `css-mode',
and START and END are the limits of the chunk.

Or, maybe return nil if not sure what mode it should be.
I don't know. The doc is thin and the code is impenetrable.

This method attempts to cache the calculated result and use it
intelligently.  For example if the first execution determines
that the POS is within a C# chunk, the limits of that chunk
are cached. If a subsequent invocation of this method provides a
POS that is within those limits, the function can safely return
the same chunk response, without further scanning.

This works as long as the buffer hasn't changed - in other words
it's just cursor navigation.
"

  ;; If we're in the right zone, then use the cached value.
  ;; Don't use the cache if it is HTML mode, because an HTML
  ;; chunk can contain a javascript chunk, a CSS chunk, a
  ;; csharp chunk.
  (if (and aspx-mode--last-chunk-result
           (> pos (nth 1 aspx-mode--last-chunk-result))
           (< pos (nth 2 aspx-mode--last-chunk-result))
           (not (eq 'html-mode (nth 0 aspx-mode--last-chunk-result))))
      (progn
        (aspx-mode-log 3 "determine-chunk: pos %d chunk cache %s"
                       pos
                       (prin1-to-string aspx-mode--last-chunk-result))
        aspx-mode--last-chunk-result)

    (let ((mode 'html-mode)
          (start-of-block (point-min))
          (end-of-block (point-max))
          sp ep
          new-result)

      (save-excursion
        (save-restriction
          (widen)
          (goto-char pos)
          (cond

           ;; Between <script language='javascript' ..> and </script>?
           ((save-excursion
              (and (and (re-search-backward aspx-mode-browser-script-start-re nil t)
                        (setq sp (match-end 0)))
                   (and (re-search-forward "</\\(script\\|SCRIPT\\)>" nil t)
                        (setq ep (line-beginning-position)))
                   (> ep pos)))

            (setq
             ;;mode 'javascript-mode
             mode 'espresso-mode
             start-of-block sp
             end-of-block (1- ep) ))


           ;; Between <style type="text/css"> and </style>?
           ((save-excursion
              (and (and (re-search-backward aspx-mode-css-block-start-re nil t)
                        (setq sp (match-end 0)))
                   (and (re-search-forward "</\\(style\\|style\\)>" nil t)
                        (setq ep (line-beginning-position)))
                   (> ep pos)))
            (setq mode 'css-mode
                  start-of-block sp
                  end-of-block (1- ep) ))


           ;; Between <script language='??'  runat='server'> and </script>?
           ((save-excursion
              (and (and (re-search-backward aspx-mode-server-script-start-re nil t)
                        (setq sp (match-end 0)))
                   (and (re-search-forward "</\\(script\\|SCRIPT\\)>" nil t)
                        (setq ep (line-beginning-position)))
                   (> ep pos)))

            ;; TODO: support VBNET-mode, too.  Check the language at the
            ;; start block.
            (setq mode 'csharp-mode
                  start-of-block sp
                  end-of-block (1- ep) ))

           ;; Between <%@ Page...>  and the first <html>
           ((save-excursion
              (and (and (re-search-forward "<\\(html\\|HTML\\)>" nil t)
                        (setq ep (line-beginning-position)))
                   (> ep pos)))

            ;; TODO: support VBNET-mode, too.  Check the specified language at the
            ;; start block.
            ;; This works only because csharp-mode has smarts to fontify the
            ;; @Page directive.
            (setq mode 'csharp-mode
                  start-of-block 1
                  end-of-block (1- ep) ))



           ;;              ;; Between <html..> and </html>
           ;;              ((save-excursion
           ;;                 (and (and (re-search-backward "<\\(HTML\\|html\\)>" nil t)
           ;;                           (setq sp (match-beginning 0)))
           ;;                      (and (re-search-forward "</\\(html\\|HTML\\)>" nil t)
           ;;                           (setq ep (line-end-position)))
           ;;                      (> ep pos)))
           ;;               (setq mode 'html-mode
           ;;                     start-of-block sp
           ;;                     end-of-block (1- ep) ))

           (t
            nil))))

      ;; multi-make-list does not actually make a new list.
      ;; Instead it destructively modifies the existing list.
      ;; The doc says it wants to avoid producing a cons cell
      ;; in the post-command-hook.
      ;; Therefore, to cache the result, we need to actually
      ;; cons a distinct list.  To check that the new item is
      ;; distinct, we need to compare each elt in the list.
      ;; If that's the case, start a timer.
      (setq new-result (list mode start-of-block end-of-block))

      (if (or (not (eq (nth 0 new-result) (nth 0 aspx-mode--last-chunk-result)))
              (not (eq (nth 1 new-result) (nth 1 aspx-mode--last-chunk-result)))
              (not (eq (nth 2 new-result) (nth 2 aspx-mode--last-chunk-result))))
          (progn
            (aspx-mode-log 3 "new chunk, restart timer")
            (aspx-mode-restart-timer)))

      (setq aspx-mode--last-chunk-result
            (multi-make-list mode start-of-block end-of-block))

      )))



(defun aspx-mode ()
  "Mode for editing ASPX files with embedded C# script blocks,
as well as CSS, Javascript, and HTML.
"
  (interactive)
  (set (make-local-variable 'multi-mode-alist)
       ;; This is a very odd data structure. It doesn't make sense that
       ;; it is formatted this way. The documentation is completely
       ;; unhelpful.
       '(
         (csharp-mode      . aspx-mode-determine-current-chunk)
         (espresso-mode    . nil)  ;; javascript
         (css-mode         . nil)
         (html-mode        . nil)
         ))
  (add-hook 'after-change-functions 'aspx-mode-after-change-fn nil t)
  (multi-mode-install-modes))





(provide 'aspx-mode)
