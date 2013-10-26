;;; org-mode-crate.el - A pre-defined org environment for the consummate gtd'er
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
;; Commentary:
;; Refer to installation instructions in the README document.
;;
;;; Code:


(when (not (boundp 'org-directory))
  (error "org-directory is unset. Please refer to instructions in the README"))
(message "`org-directory' has been set to: %s" org-directory)


(provide 'org-mode-crate)
