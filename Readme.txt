Mon, 24 May 2010  17:21

This is the readme for csharp-mode.

You can use csharp-mode alone.  To do so,


 put this in your .emacs:

   (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

 or:

   (require 'csharp-mode)


 AND:

   (setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
   (defun my-csharp-mode-fn ()
      "function that runs when csharp-mode is initialized for a buffer."
      ...insert your code here...
      ...most commonly, your custom key bindings ...
   )
   (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)


=======================================================

You can also take advantage of C# code completion.
To do so, put csharp-completion.el, csharp-shell.el , and powershell.el
on your load-path.

You must also have semantic, from the CEDET package, on your load path.

Put the CscompUtilities.dll in the same location as csharp-shell.el.

Put this in your .emacs file:


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; C# code completion (requires CEDET semantic)

    (setq load-path
          (append '("~/users/dinoch/elisp/cedet/semantic"
                    "~/users/dinoch/elisp/cedet/semantic/bovine"
                    "~/users/dinoch/elisp/cedet/common"
                    "~/users/dinoch/elisp/cedet/eieio"
                    "~/users/dinoch/elisp/cedet/contrib"
                    )  load-path ))

    (load "semantic")
    (load "semantic-load")
    (load "wisent-csharp")

    (require 'csharp-completion)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


In your csharp-mode-hook, bind a key to the cscomp completion trigger.
Like this:

         ;; C# code completion
         (local-set-key "\M-\\"   'cscomp-complete-at-point)
         (local-set-key "\M-\."   'cscomp-complete-at-point-menu)



