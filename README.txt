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

* Caveats
 - This configuration is ONLY known to work with Emacs 24. You are on
   your own with older versions of Emacs.
