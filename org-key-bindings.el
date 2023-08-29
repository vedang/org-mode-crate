;;; org-key-bindings --- Key bindings in Org
;;
;;; Copyright (C) 2012, 2013 Vedang Manerikar
;;
;; Author: Vedang Manerikar <vedang.manerikar@gmail.com>
;; Created on: 13 Dec 2012
;; Keywords: configuration, org-mode
;; URL: https://github.com/vedang/org-mode-crate
;; Version: DEV
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; Refer to installation instructions in the README document.
;;
;;; Code:

(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-M-x") 'org-toggle-link-display)
(global-set-key (kbd "C-c t") 'org-todo)
(global-set-key (kbd "<f6>") 'org-capture)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "<S-f5>") 'widen)
(global-set-key (kbd "C-<f11>") 'org-clock-goto)
(global-set-key (kbd "C-c C-x C") 'org-clone-subtree-with-time-shift)
(define-key org-mode-map (kbd "C-c C-x C") 'org-clone-subtree-with-time-shift)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (outline-hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

;; @TODO: Remove `bh/hide-other' in the future if `outline-hide-other'
;; proves to be better.

;; (global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> h") 'outline-hide-other)

(provide 'org-key-bindings)
;;; org-key-bindings.el ends here
