Created Mon, 24 May 2010  17:21
Updated Fri, 13 May 2011  13:02

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

The c# code completion is undergoing  some changes.
For now, don't use it.

=======================================================

x-URL: http://code.google.com/p/csharpmode/

