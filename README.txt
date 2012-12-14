-*- mode: org; -*-

* Org Mode Crate

This repository contains org-mode and my configuration for it as a
plug-and-play module.

I've strived to make this configuration super simple to install and
use, if you face any problems please open an issue and let me know
about it.

* INSTALL
** Get the code:
   - git clone --recursive git://github.com/vedang/org-mode-crate.git ~/.emacs.d/path/to/org-mode-crate

** Add the following lines to your .emacs

   (add-to-list 'load-path "/path/to/org-mode-crate/")
   (setq org-directory "/path/to/org-files/")
   (require 'org-mode-crate-init)

** Press F12 to get started with your agenda

* Workflow notes
 - Create and maintain as many files in your org-directory as you want.
 - Currently, I don't handle recursively loading org files, so your
   org-directory should be a flat structure.
 - Tasks are categorized as follows:
   - Important tasks - These tasks should be tagged with one (or more)
     of the following tags: next, release, imp
   - Other tasks that should be tracked - These tasks should be tagged
     with one (or more) of the following tags: fun, future, productive

* Caveats
 - This configuration is ONLY known to work with Emacs 24. You are on
   your own with older versions of Emacs.
