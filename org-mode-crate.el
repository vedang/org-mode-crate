;;; org-mode-crate --- A pre-defined org environment for the consummate gtd'er
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


(when (not (boundp 'org-directory))
  (error "org-directory is unset. Please refer to instructions in the README"))
(message "`org-directory' has been set to: %s" org-directory)


(require 'org-agenda)
(require 'org-key-bindings)
(require 'org-checklist)

;; Setup directory and file paths for org
(setq org-archive-directory (concat org-directory "/archive")
      org-archive-location (concat org-archive-directory "/%s_archive::")
      org-default-notes-file (concat org-directory "/refile.org")
      org-agenda-files (list org-directory))

;; settings for org which don't fall under any particular category.
(setq org-special-ctrl-a/e t
      org-imenu-depth 4)


;; Ido for the win
(setq org-completion-use-ido t)


;; Auto starting org-mode for following file types
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(transient-mark-mode 1)

;; Undefine C-c [ and C-c ] since this breaks my org-agenda files
;; when directories are included
;; It expands the files in the directories individually
(add-hook 'org-mode-hook (lambda ()
                           (turn-on-font-lock)
                           (org-defkey org-mode-map "\C-c[" 'undefined)
                           (org-defkey org-mode-map "\C-c]" 'undefined)))


;;; Settings for org-capture
;; Tip: Add the following to the top of your `org-default-notes-file'
;; to find tasks that need refiling via a simple tag search:
;; #+FILETAGS: refile
(setq org-capture-templates
      '(("t" "Todo"
         entry (file org-default-notes-file)
         "* TODO %^{What do I want to do} \n%U\n%a\n %i%?"
         :clock-in t
         :clock-resume t)
        ("r" "Respond to email"
         entry (file org-default-notes-file)
         "* TODO Respond to %:from on %:subject  :email: \nSCHEDULED: %t\n%U\n%a\n"
         :clock-in t
         :clock-resume t
         :immediate-finish t)
        ("n" "Note"
         entry (file org-default-notes-file)
         "* %?  :notes:\n%U\n%a\n  %i" :clock-in t :clock-resume t)
        ("p" "Org Protocol"
         entry (file org-default-notes-file)
         "* TODO Review %c\n%U\n"
         :immediate-finish t)
        ("l" "Link"
         entry (file org-default-notes-file)
         "* Review [[%c][%? ]]  :linklog:\n%U\n"
         :clock-in t
         :clock-resume t))
      org-datetree-add-timestamp t)


;;; Refile settings
(setq org-refile-targets '((org-agenda-files :maxlevel . 9)
                           (nil :maxlevel . 9))
      ;; Targets start with the file name - allows creating level 1 tasks
      org-refile-use-outline-path 'file
      ;; Show the entire path in one step, ido will help me filter
      ;; results and get to where I want to go.
      org-outline-path-complete-in-steps nil
      ;; Make it possible for me to create a new sub-heading under
      ;; which I want to refile something.
      org-refile-allow-creating-parent-nodes 'confirm
      ;; File new notes, refile new todos on top instead of at the
      ;; bottom
      org-reverse-note-order t
      ;; Exclude DONE state tasks from refile targets
      org-refile-target-verify-function 'bh/verify-refile-target)

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;;; org-todo settings
;; keys mentioned in brackets are hot-keys for the States
;; ! indicates insert timestamp
;; @ indicates insert note
;; / indicates entering the state
(setq org-todo-keywords
      '((sequence "TODO(t!/!)" "WORKING(w!/!)" "|" "DONE(d!/@)")
        (sequence "FOLLOWUP(f!/!)" "WAITING(a@/!)" "DELEGATED(e@/!)" "|" "CANCELLED(c@/!)")
        (sequence "PROJECT(p)" "|" "MEETING(m!/!)" "SOMEDAY(S)" "RESTRUCTURED(r@/!)")))


(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("WORKING" :foreground "orange" :weight bold)
        ("DONE" :foreground "SeaGreen4" :weight bold)
        ("FOLLOWUP" :foreground "IndianRed4" :weight bold)
        ("WAITING" :foreground "lightblue" :weight bold)
        ("DELEGATED" :foreground "IndianRed1" :weight bold)
        ("CANCELLED" :foreground "SeaGreen4" :weight bold)
        ("PROJECT" :foreground "light slate blue" :weight bold)
        ("MEETING" :foreground "forest green" :weight bold)
        ("SOMEDAY" :foreground "magenta" :weight bold)
        ("RESTRUCTURED" :foreground "SeaGreen4" :weight bold)))


;; Changing State should trigger following Tag changes
(setq org-todo-state-tags-triggers
      '(("SOMEDAY"
         ("waiting" . t) ("next" . nil))
        (done
         ("next" . nil) ("waiting" . nil) ("followup" . nil))
        ("WAITING"
         ("next" . nil) ("waiting" . t))
        ("TODO"
         ("waiting" . nil) ("followup" . nil) ("cancelled" . nil) ("next" . nil))
        ("FOLLOWUP"
         ("followup" . t))
        ("CANCELLED"
         ("next" . nil) ("followup" . nil) ("cancelled" . t))
        ("WORKING"
         ("waiting" . nil) ("next" . t))))


(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "waiting")
         t)
        ((string= tag "errand")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)


;; Other todo related settings
(setq org-use-fast-todo-selection t
      org-fast-tag-selection-single-key 'expert
      ;; Allow me to change state without it being logged
      org-treat-S-cursor-todo-selection-as-state-change nil
      ;; show TODO counts of _all_ subtasks under a heading
      org-hierarchical-todo-statistics nil
      ;; Ensure that we can only mark a task as complete when
      ;; sub-tasks and ordered tasks are complete.
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)


(dolist (map (list org-agenda-keymap org-agenda-mode-map))
  (define-prefix-command 'org-todo-state-map)
  (define-key map "x" 'org-todo-state-map)

  (define-key org-todo-state-map "d"
    #'(lambda nil (interactive) (org-agenda-todo "DONE")))
  (define-key org-todo-state-map "x"
    #'(lambda nil (interactive) (org-agenda-todo "CANCELLED")))

  ;; These functions are defined later in the file.
  (define-key org-todo-state-map "D" #'fc/org-agenda-inherit-deadline))


;;org-tags
;; Important Tag list
(setq org-tag-alist
      '(("next" . ?x)
        ("notes" . ?n)
        ("important" . ?i)
        ("action_items" . ?a)
        ("waiting" . ?w)))


;; org-priorities
(setq org-lowest-priority ?E)
(setq org-default-priority ?D)


;; Logbook settings
(setq org-log-done 'time
      org-log-into-drawer t
      org-log-reschedule 'note
      org-log-redeadline 'note)


;; org-list
(setq org-list-demote-modify-bullet
      '(("+" . "-") ("-" . "+") ("*" . "+")))

;; From @suvrat to keep touch-typing when working with lists.
(defun suv/org-move-item-or-tree ()
  (interactive)
  (message "Use f, b, n, p to move individual items. Use C-{f,b,n,p} for point movement.")
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'org-shiftmetaright)
    (define-key map (kbd "b") 'org-shiftmetaleft)
    (define-key map (kbd "n") 'org-metadown)
    (define-key map (kbd "p") 'org-metaup)
    (define-key map (kbd "C-f") 'forward-char)
    (define-key map (kbd "C-b") 'backward-char)
    (define-key map (kbd "C-n") 'next-line)
    (define-key map (kbd "C-p") 'previous-line)
    (set-transient-map map t)))

(define-key org-mode-map (kbd "C-c j") 'suv/org-move-item-or-tree)

;;; org-clock
(org-clock-persistence-insinuate)
(setq org-clock-history-length 20
      org-clock-in-resume t
      org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK")
      org-clock-into-drawer "CLOCK"
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t
      org-clock-persist t
      org-clock-persist-file (concat org-directory "/org-clock-save")
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      org-clock-report-include-clocking-task t)


;; List of TODO states to clock-in
(setq vm/todo-list '("TODO" "WAITING"))

;; Change task state to WORKING when clocking in
(defun bh/clock-in-to-working (kw)
  "Switch task from TODO to WORKING when clocking in.
Skips capture tasks and tasks with subtasks"
  (when (and (not (and (boundp 'org-capture-mode) org-capture-mode))
             (member kw vm/todo-list))
    "WORKING"))

(setq org-clock-in-switch-to-state 'bh/clock-in-to-working)


;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


(setq bh/keep-clock-running nil)

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ;; Find the tags on the current task
      (if (and (equal major-mode 'org-mode)
               (not (org-before-first-heading-p))
               (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(global-set-key (kbd "<f9> i") 'bh/punch-in)


(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(global-set-key (kbd "<f9> o") 'bh/punch-out)


(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
                       (org-clock-in))))


(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
                               (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(require 'org-id)
;;; Ensuring sane defaults for `org-id'
(setq org-id-track-globally t)

(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (when (boundp 'bh/organization-task-id)
    (org-with-point-at (org-id-find bh/organization-task-id 'marker)
                       (org-clock-in '(16)))))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))


(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)


;; org-agenda
;; Custom views for Agenda
(setq org-agenda-custom-commands
      '(("g" "GTD Agenda"
         ((tags-todo "+important"
                     ((org-agenda-overriding-header
                       "These are your IMPORTANT Tasks")
                      (org-agenda-dim-blocked-tasks 'invisible)
                      ;; Sorting is *really* slowing it down.

                      ;; @TODO: Figure out a way to speed this up,
                      ;; maybe by specifying certain files here and
                      ;; creating a separate custom agenda for all
                      ;; important tasks.
                      ;; (org-agenda-sorting-strategy
                      ;;  '(timestamp-down effort-up))
                      ))
          (agenda ""
                  ((org-agenda-overriding-header
                    "Your Meetings today")
                   (org-agenda-entry-types '(:timestamp :sexp))
                   (org-agenda-repeating-timestamp-show-all t)
                   (org-agenda-time-grid '((daily today require-timed)
                                           (800 1000 1200 1400 1600 1800 2000)
                                           "......" "----------------"))))
          (agenda ""
                  ((org-agenda-overriding-header
                    "These are your URGENT Tasks")
                   (org-agenda-entry-types '(:deadline))
                   (org-deadline-warning-days 1)
                   (org-agenda-sorting-strategy '(habit-down priority-down timestamp-down))))
          ))
        ("n" "Your NEXT Tasks" tags-todo "+next")
        ("h" "Your Habits" tags-todo "STYLE=\"habit\"")
        ("r" "Refile" tags "+refile")))
;;; Don't recalculate agenda unless I explicitly say so.
(setq org-agenda-sticky t)

;;; Show the Habit graph in all Agenda buffers (where Habits are
;;; available). From:
;;; https://emacs.stackexchange.com/a/17328/20448

(defvar so/org-habit-show-graphs-everywhere t
  "If non-nil, show habit graphs in all types of agenda buffers.

Normally, habits display consistency graphs only in
\"agenda\"-type agenda buffers, not in other types of agenda
buffers.  Set this variable to any non-nil variable to show
consistency graphs in all Org mode agendas.")

(defun so/org-agenda-mark-habits ()
  "Mark all habits in current agenda for graph display.

This function enforces `so/org-habit-show-graphs-everywhere' by
marking all habits in the current agenda as such.  When run just
before `org-agenda-finalize' (such as by advice; unfortunately,
`org-agenda-finalize-hook' is run too late), this has the effect
of displaying consistency graphs for these habits.

When `so/org-habit-show-graphs-everywhere' is nil, this function
has no effect."
  (when (and so/org-habit-show-graphs-everywhere
             (not (get-text-property (point) 'org-series)))
    (let ((cursor (point))
          item data)
      (while (setq cursor (next-single-property-change cursor 'org-marker))
        (setq item (get-text-property cursor 'org-marker))
        (when (and item (org-is-habit-p item))
          (with-current-buffer (marker-buffer item)
            (setq data (org-habit-parse-todo item)))
          (put-text-property cursor
                             (next-single-property-change cursor 'org-marker)
                             'org-habit-p data))))))

(advice-add #'org-agenda-finalize :before #'so/org-agenda-mark-habits)

;;; http://article.gmane.org/gmane.emacs.orgmode/49215
(defun fc/has-inheritable-deadline-p ()
  "Any task (without DEADLINE) that can inherit a DEADLINE"
  (let ((deadline (org-entry-get nil "DEADLINE"))
        (inheritable-deadline (org-entry-get-with-inheritance "DEADLINE")))

    (if (org-not-nil deadline)
        nil
      (if (org-not-nil inheritable-deadline)
          t
        nil))))


(defun fc/skip-non-inheritable-deadlines ()
  "Skip tasks that cannot inherit a DEADLINE"
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (if (fc/has-inheritable-deadline-p)
        nil
      next-headline)))


(defun fc/org-inherit-deadline ()
  "Inherit a DEADLINE."
  (interactive)
  (let* ((deadline (org-entry-get-with-inheritance "DEADLINE")))
    (if (and (org-not-nil deadline)
             (y-or-n-p (format "Inherit DEADLINE: <%s>? " deadline)))
        (org-deadline nil (org-time-string-to-time deadline)))))


(defun fc/org-agenda-inherit-deadline (&optional arg)
  "Inherit a DEADLINE in agenda."
  (interactive "P")
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (hdmarker (or (org-get-at-bol 'org-hd-marker)
                       marker))
         (pos (marker-position marker))
         newhead)
    (org-with-remote-undo (marker-buffer marker)
                          (with-current-buffer (marker-buffer marker)
                            (widen)
                            (goto-char pos)
                            (org-show-context 'agenda)
                            (org-show-entry)
                            (org-cycle-hide-drawers 'children)
                            (fc/org-inherit-deadline)
                            (setq newhead (org-get-heading)))
                          (org-agenda-change-all-lines newhead hdmarker))))


;; Always highlight current agenda line
(add-hook 'org-agenda-mode-hook '(lambda ()
                                   (hl-line-mode 1)))

(setq org-agenda-show-all-dates nil
      org-agenda-start-on-weekday 1
      org-agenda-time-grid
      '(nil (800 1000 1200 1400 1600 1800 2000)
            "......"
            "----------------")
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-text-search-extra-files '(agenda-archives)
      org-agenda-log-mode-items '(clock)
      org-agenda-clockreport-parameter-plist '(:link t
                                                     :maxlevel 5
                                                     :fileskip0 t
                                                     :compact t
                                                     :narrow 80)
      org-agenda-span 'day
      org-columns-default-format
      "%50ITEM(Task) %5Effort(Effort){:} %5CLOCKSUM %3PRIORITY %20DEADLINE %20SCHEDULED %20TIMESTAMP %TODO %CATEGORY(Category) %TAGS"
      org-global-properties
      '(("Effort_ALL" . "0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00")
        ("STYLE_ALL" . "habit"))
      org-agenda-clock-consistency-checks
      '(:max-duration "4:00" :min-duration 0 :max-gap 0 :gap-ok-around ("4:00")))


;; settings for Reminder
;; Erase all reminders and rebuild reminders for today from the agenda
;; (defadvice org-agenda-to-appt (before wickedcool activate)
;;   "Clear the appt-time-msg-list."
;;   (setq appt-time-msg-list nil))

;; (add-hook 'org-agenda-finalize-hook 'org-agenda-to-appt)

;; (appt-activate t)

;; ;; If we leave Emacs running overnight -
;; ;; reset the appointments one minute after midnight
;; (run-at-time "24:01" nil 'org-agenda-to-appt)


;; Settings for org-table
;; Export org table as CSV by default
(setq org-table-export-default-format "orgtbl-to-csv")


(setq org-link-abbrev-alist
      '(("google"   . "http://www.google.com/search?q=%s")))


;; Enable some extra modules by modifying org modules to load
(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'ol-man)
(add-to-list 'org-modules 'ol-git-link)
(add-to-list 'org-modules 'ol-notmuch)

;; Pull in contrib export backends that I want
(require 'ox-md)
(require 'ox-confluence)
(require 'ox-taskjuggler)
(dolist (b (list 'beamer 'md 'confluence 'taskjuggler))
  (add-to-list 'org-export-backends b))


;; Add Babel execution support for es-mode, if it has been installed.
(eval-after-load 'ob-elasticsearch
  '(progn (org-babel-do-load-languages
           'org-babel-load-languages
           '((elasticsearch . t)))))

;; Add Babel execution support for shell
(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)
                               (emacs-lisp . t)
                               (plantuml . t)))

;;; Structural Editing
;; Modify functions found in org-list.el for my purposes

(defun vedang/org-list-top-level-to-subtree (list &optional params)
  "Convert LIST into an Org subtree.
LIST is as returned by `org-list-to-lisp'.  PARAMS is a property
list with overruling parameters for `org-list-to-generic'."
  (let* ((blank (pcase (cdr (assq 'heading org-blank-before-new-entry))
                  (`t t)
                  (`auto (save-excursion
                           (org-with-limited-levels (outline-previous-heading))
                           (org-previous-line-empty-p)))))
         (level (org-reduced-level (or (org-current-level) 0)))
         (make-heading/list-prefix
          (lambda (_type depth &optional _count)
            ;; Return the string for the heading, depending on DEPTH
            ;; of current sub-list.
            (if (= 1 depth)
                (concat (make-string (if org-odd-levels-only
                                         (1- (* 2 (+ level 1)))
                                       (+ level 1))
                                     ?*)
                        " ")
              (if (= 2 depth)
                  (concat (make-string (if org-odd-levels-only
                                           (1- (* 2 (+ level depth)))
                                         (+ level depth))
                                       ? )
                          "- ")
                (concat (make-string (if org-odd-levels-only
                                         (1- (* 2 (+ level
                                                     (* 2  (- depth 1)))))
                                       (+ level
                                          (* 2 (- depth 1))))
                                     ? )
                        (if (oddp depth)
                            "+ "
                          "- ")))))))
    (org-list-to-generic
     list
     (org-combine-plists
      (list :splice t
            :istart make-heading/list-prefix
            :icount make-heading/list-prefix
            :dtstart " "
            :dtend " "
            :isep (if blank "\n\n" "\n")
            :cbon "[X] "
            :cboff "[ ] "
            :cbtrans "[/] ")
      params))))

(defun vedang/org-list-make-top-level-subtree ()
  "Convert the plain list at point into a subtree."
  (interactive)
  (if (not (ignore-errors (goto-char (org-in-item-p))))
      (error "Not in a list")
    (let ((list (org-list-to-lisp t)))
      (save-excursion (insert (vedang/org-list-top-level-to-subtree list))))))

(define-key org-mode-map (kbd "C-c C-*")
  'vedang/org-list-make-top-level-subtree)


(provide 'org-mode-crate)
;; A big thanks to Bernt Hansen for providing an awesome guide to
;; beginners so that we can harness the power of org-mode. Almost all of the
;; customization here, and my complete day-to-day workflow,
;; is based on his document about org-mode which can be
;; found here: http://doc.norang.ca/org-mode.html
