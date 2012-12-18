;;; org-mode-crate-init.el --- Load code for a better clojure environment
;;; Author: Vedang Manerikar
;;; Created on: 13 Dec 2012
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Commentary:
;; To use this code, simply add
;;    (add-to-list 'load-path "/path/to/org-mode-crate/")
;;    (setq org-directory "path/to/org-files-dir")
;;    (require 'org-mode-crate-init)
;; to your .emacs file

;;; Code:


(when (not (boundp 'org-directory))
  (error "org-directory is unset. Please refer to instructions in the README"))

(message "Checked for org-directory.")


(defun vedang/list-dirs-recursively (directory)
  "List all the directories in DIRECTORY and in it's sub-directories."
  (let* ((current-directory-list (directory-files-and-attributes directory t))
         (dirs-list (delq nil (mapcar (lambda (lst)
                                        (and (car (cdr lst))
                                             (not (equal "." (substring (car lst) -1)))
                                             (not (equal ".#" (substring (car lst) -2)))
                                             (not (equal ".git" (substring (car lst) -4)))
                                             (car lst)))
                                      current-directory-list))))
    (apply #'append dirs-list (mapcar (lambda (d)
                                        (vedang/list-dirs-recursively d))
                                      dirs-list))))


(defun vedang/add-dirs-to-load-path (directory)
  "Add all directories under the input directory to the emacs load path"
  (dolist (dirname (vedang/list-dirs-recursively directory))
    (add-to-list 'load-path dirname)))


(vedang/add-dirs-to-load-path (file-name-directory
                               (or (buffer-file-name) load-file-name)))


;; Agenda does not have a key-binding by default in Emacs 24.
;; Provide one as a starting point.
(global-set-key (kbd "<f12>") 'org-agenda)


(eval-after-load "org"
  '(progn
     (require 'org-key-bindings)
     (require 'org-config)))


(message "Press <f12> to get started with your agenda...")
(provide 'org-mode-crate-init)
