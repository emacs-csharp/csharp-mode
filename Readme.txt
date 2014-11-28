BRANCH NOTE: ORIGINAL BRANCH CONTENT (mostly) AS FOUND ON GOOGLE CODE.

This branch was created mostly to have a place where old "cruft" can be
kept around in case we should ever wish to revive it.

Currently this includes everything originally published, and not just the
data relevant to getting the csharp-mode package published.


Original read-content found below:


Created Mon, 24 May 2010  17:21
Updated Fri, 13 May 2011  13:02

x-URL: http://code.google.com/p/csharpmode/

This is the readme for csharp-mode.

=======================================================

You can use csharp-mode just as it is.  To do so,

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

The c# code completion is experimental. It depends on
ICSharpCode.NRefactory.dll,  as well as powershell.el and
a few other .el modules.

Use it and let me know how it goes for you.


=======================================================


