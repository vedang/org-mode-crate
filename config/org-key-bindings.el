;;; org-key-bindings.el --- Standard org-mode key bindings
;;; Author: Vedang Manerikar
;;; Created on: 13 Dec 2012
;;; Time-stamp: "2012-12-14 17:21:49 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c t") 'org-todo)
(global-set-key (kbd "C-c C-r") 'org-capture)
(global-set-key (kbd "<f6>") 'org-capture)
(global-set-key (kbd "<S-f5>") 'widen)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)


(provide 'org-key-bindings)
