;;; org-mode-crate --- A pre-defined org environment for the consummate gtd'er -*- lexical-binding: t -*-
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
  (error "The variable `org-directory' is unset. Please refer to instructions in the README"))

(message "`org-directory' has been set to: %s" org-directory)

(require 'org-agenda)
(require 'org-key-bindings)
(require 'org-checklist)

;; Setup directory and file paths for org
(defvar org-archive-directory (concat org-directory "/archive")
  "Directory under which all archived content is stored.")

(setq org-archive-location (concat org-archive-directory "/%s_archive::")
      org-default-notes-file (concat org-directory "/daily.org")
      org-agenda-files (list org-directory))

;;; Default definitions for variables used in capture templates
(when (not (boundp 'org-journal-file))
  (defvar org-journal-file org-default-notes-file
    "File in which journal entries are stored.

Check-ins and feedback are also stored here."))
(when (not (boundp 'org-blogpost-file))
  (defvar org-blogpost-file org-default-notes-file
    "File in which blogposts and microblogposts are stored."))
(when (not (boundp 'org-company-file))
  (defvar org-company-file org-default-notes-file
    "File in which company documentation is stored."))

;; Settings for org which don't fall under any particular category.
(setq org-fold-catch-invisible-edits 'show-and-error
      org-imenu-depth 4
      org-insert-heading-respect-content t
      org-special-ctrl-a/e nil
      org-auto-align-tags nil
      org-tags-column 0
      org-use-sub-superscripts '{}
      ;; Org styling, hide markup etc.
      org-hide-emphasis-markers t
      org-pretty-entities t)

;; Ellipsis styling
(setq org-ellipsis "⤵")
(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

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
(require 'org-protocol)
(require 'org-capture)

;;; *CRITICAL NOTE* Read before modifying the push stack below:
;; Pushing to capture templates is a stack. What goes in first shows
;; up at the bottom of the capture templates list.

;;; Templates for thinking tools
(push '("T" "Templates for Helping Me Think") org-capture-templates)

;; Capture a decision that you've taken, for review and reflection later.
(push `("Td" "Decision Journal" entry
        (file+headline org-default-notes-file "Helping Me Think")
        (file ,(expand-file-name "capture-templates/thinking.decision.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; Create a Current Reality Tree for a problem
(push `("Tc" "Current Reality Tree" entry
        (file+headline org-default-notes-file "Helping Me Think")
        (file ,(expand-file-name "capture-templates/thinking.crt.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; Create an Evaporating Cloud for a problem
(push `("Te" "Evaporating Cloud" entry
        (file+headline org-default-notes-file "Helping Me Think")
        (file ,(expand-file-name "capture-templates/thinking.ec.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; Create a Future Reality Tree for a problem
(push `("Tf" "Future Reality Tree" entry
        (file+headline org-default-notes-file "Helping Me Think")
        (file ,(expand-file-name "capture-templates/thinking.frt.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; Create a Prerequisite Tree for a problem
(push `("Tp" "Prerequisite Tree" entry
        (file+headline org-default-notes-file "Helping Me Think")
        (file ,(expand-file-name "capture-templates/thinking.prt.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; Create a Transition Tree for a problem
(push `("Tt" "Transition Tree" entry
        (file+headline org-default-notes-file "Helping Me Think")
        (file ,(expand-file-name "capture-templates/thinking.trt.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; Capture a new Business idea for sketching out / thinking through
(push `("Tb" "Business Canvas" entry
        (file+headline org-default-notes-file "Helping Me Think")
        (file ,(expand-file-name "capture-templates/business.canvas.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; Capture a customer persona, note that this is always captured in
;; the current clocking task, and is something I should do under the
;; business canvas.
(push `("TP" "Customer Persona (under Business Canvas)" entry
        (clock)
        (file ,(expand-file-name "capture-templates/business.customer.persona.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; Capture a customer journey through your product, note that this is
;; always captured in the current clocking task
(push `("Tj" "Customer Journey (under Business Canvas)" entry
        (clock)
        (file ,(expand-file-name "capture-templates/business.customer.journey.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;;; Templates for capturing data about myself on a day-to-day basis
(push '("d" "Templates for Capturing Data (personal)") org-capture-templates)

;; Capture weight / food. This seems hard to get into a laptop habit.
;; This is the kind of quantitative life that a mobile solution would
;; have helped with.

(push `("dw" "Weight Tracking" entry
        (file+olp+datetree org-default-notes-file)
        (file ,(expand-file-name "capture-templates/bodylog.weight.org"))
        :clock-in t
        :clock-resume t
        :immediate-finish t
        :empty-lines 1)
      org-capture-templates)

(push `("df" "Food Tracking" entry
        (file+olp+datetree org-default-notes-file)
        (file ,(expand-file-name "capture-templates/bodylog.food.org"))
        :clock-in t
        :clock-resume t
        :immediate-finish t
        :empty-lines 1)
      org-capture-templates)

(push `("dd" "Downtime Tracking" entry
        (file+olp+datetree org-default-notes-file)
        (file ,(expand-file-name "capture-templates/bodylog.dt.org"))
        :clock-in t
        :clock-resume t
        :immediate-finish t
        :empty-lines 1)
      org-capture-templates)

;;; Templates for capturing build in public ideas
(push '("b" "Templates for Capturing Build in Public") org-capture-templates)

;; Capture Micro-blogging
(push `("bm" "New Microblogging entry" entry
        (file+olp+datetree org-blogpost-file "Microblogging")
        (file ,(expand-file-name "capture-templates/microblog.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; New blogpost idea
(push `("bb" "New Blogpost entry" entry
        (file+headline org-blogpost-file "Meta: Blogposts to write")
        (file ,(expand-file-name "capture-templates/todo.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;;; Templates for when I want to capture specific feedback about something
(push '("f" "Templates for Feedback, Reflection, Journaling") org-capture-templates)

;; Capture feedback for people I am working with
(push `("fp" "Feedback for People I'm working with" item
        (file+headline org-company-file "Feedback")
        (file ,(expand-file-name "capture-templates/feedback.others.org"))
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; The monthly newsletter to send to investors, friends and mentors
(push `("fn" "Company Newsletters" entry
        (file+headline org-company-file "Company Newsletters")
        (file ,(expand-file-name "capture-templates/business.updates.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; Capture suggestions / ideas from other people, which can be
;; expanded into actual projects later.
(push `("fs" "Ideas and Suggestions" entry
        (file+headline org-company-file "Ideas and Suggestions")
        (file ,(expand-file-name "capture-templates/suggestion.org"))
        :prepend t
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;; Capture personal check-ins, journaling
(push `("fc" "Personal Check In or Journaling" entry
        (file+olp+datetree org-journal-file)
        (file ,(expand-file-name "capture-templates/journaling.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;;; Templates for planning on a day-to-day basis
(push '("p" "Templates for Planning") org-capture-templates)

;; Deliberately plan out and make a routine out of start of day and
;; end of day activities.

(push `("ps" "The Start of Day Planning Routine" entry
        (file+olp+datetree org-default-notes-file)
        (file ,(expand-file-name "capture-templates/workday.start.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

(push `("pe" "The End of Day Reflection Routine" entry
        (file+olp+datetree org-default-notes-file)
        (file ,(expand-file-name "capture-templates/workday.end.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

(push `("pn" "The Next Day Intentions Routine" entry
        (file+olp+datetree org-default-notes-file)
        (file ,(expand-file-name "capture-templates/workday.next.org"))
        :prepend nil
        :clock-in t
        :clock-resume t
        :empty-lines 1)
      org-capture-templates)

;;; Templates for capturing meetings, events, something happening at this time
(push '("m" "Templates for Capturing Meetings or Events") org-capture-templates)

;; Capture an upcoming meeting or one that has already happened
(push `("mp" "Meeting some other day" entry
        (file+olp+datetree org-default-notes-file)
        (file ,(expand-file-name "capture-templates/meeting.org"))
        :prepend t
        :clock-in t
        :clock-resume t
        :time-prompt t)
      org-capture-templates)

;; Capture notes for an ongoing meeting or a meeting that's already
;; happened.
(push `("mn" "Meeting today" entry
        (file+olp+datetree org-default-notes-file)
        (file ,(expand-file-name "capture-templates/meeting.org"))
        :prepend t
        :clock-in t
        :clock-resume t)
      org-capture-templates)

;;; Templates for Capturing Tasks
(push '("t" "Templates for Capturing Tasks") org-capture-templates)

;; Set up a new habit for tracking. This should be refiled to the
;; correct location later.
(push `("th" "Habit" entry
        (file+headline org-default-notes-file "My Habit Tracker")
        (file ,(expand-file-name "capture-templates/habit.org")))
      org-capture-templates)

;; One-click Capture for replying to emails from notmuch. Creates a
;; task to remind you that you need to reply to this email.
(push `("tr" "Respond to email" entry
        (file+olp+datetree org-default-notes-file)
        (file ,(expand-file-name "capture-templates/reply.org"))
        :clock-in t
        :clock-resume t
        :immediate-finish t)
      org-capture-templates)

;; One-click capture of links from the clipboard. Used in conjunction
;; with `org-protocol', or as a stand-alone to capture links.
(push `("tw" "Website Link Immediate Capture" entry
        (file+olp org-default-notes-file "Links Captured from the Browser")
        (file ,(expand-file-name "capture-templates/website.org"))
        :immediate-finish t)
      org-capture-templates)

;; A more nuanced capture for browser links, which I use for cleaning
;; out my browser 2/3 times a week.
(push `("tl" "Website Link Pinboard Capture" entry
        (file+olp org-default-notes-file "Links Captured from the Browser")
        (file ,(expand-file-name "capture-templates/pinboard.org"))
        :clock-in t
        :clock-resume t
        :immediate-finish t)
      org-capture-templates)

;; Capture a task where someone expects me to communicate when it's done
(push `("tj" "Jira or External-facing Task" entry
        (file+olp+datetree org-default-notes-file)
        (file ,(expand-file-name "capture-templates/jira.org"))
        :clock-in t
        :clock-resume t)
      org-capture-templates)

;; One-click Capture for Tasks. Captures the task immediately and gets
;; out of your way.
(push `("ti" "Simple Task Immediate Finish" entry
        (file+olp+datetree org-default-notes-file)
        (file ,(expand-file-name "capture-templates/todo.org"))
        :clock-in t
        :clock-resume t
        :immediate-finish t)
      org-capture-templates)

(require 'org-datetree)
;; add an active timestamp when you create a date-tree entry.
(setq org-datetree-add-timestamp 'active)

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
      '((sequence "TODO(t!/!)" "WORKING(w!/!)" "TESTING(T!/!)" "|" "DONE(d!/@)")
        (sequence "FOLLOWUP(f!/!)" "WAITING(a@/!)" "DELEGATED(e@/!)" "|" "CANCELLED(c@/!)")
        (sequence "PROJECT(p)" "|" "MEETING(m!/!)" "SOMEDAY(S)" "RESTRUCTURED(r@/!)")
        (sequence "TOREAD(R!/!)" "READING(E!/!)" "REREAD(A!/!)""|" "READ(D!/!)")))


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
        ("TESTING"
         ("next" . nil) ("waiting" . nil))
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


;;;; org-tags
;; Important Tag list
(setq org-tag-alist
      '((:startgrouptag)
        ("TaskStatus" . ?S)
        (:grouptags)
        ("next" . ?x)
        ("waiting" . ?w)
        ("delegated" . ?d)
        (:endgrouptag)
        (:startgrouptag)
        ("Writing" . ?W)
        (:grouptags)
        ("notes" . ?n)
        ("sketch" . ?s)
        ("feedback" . ?f)
        ("actionitems" . ?a)
        (:endgrouptag)
        (:startgrouptag)
        ("TaskType" . ?T)
        (:startgrouptag)
        ("joy" . ?j)
        ("errand" . ?e)
        ("bug" . ?b)
        ("habit" . ?h)
        ("goal" . ?g)
        (:endgrouptag)
        ("important" . ?i)
        ("refile" . ?r)
        ("future" . ?F)))

;;;; org-priorities
(setq org-lowest-priority ?E)
(setq org-default-priority ?D)

;;;; Logbook settings
(setq org-log-done 'time
      org-log-into-drawer t
      org-log-reschedule 'time
      org-log-redeadline 'time)

;;;; org-list
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

;;;; org-clock
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
             (tags (org-with-point-at marker (org-get-tags))))
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
(setq org-id-track-globally t
      org-id-link-to-org-use-id t)

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
         ((agenda ""
                  ((org-agenda-overriding-header
                    "Your Meetings today")
                   (org-agenda-entry-types '(:timestamp :sexp))
                   (org-agenda-repeating-timestamp-show-all t)
                   (org-agenda-time-grid
                    '((daily today require-timed)
                      (800 1000 1200 1400 1600 1800 2000 2200)
                      " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
                   (org-agenda-current-time-string
                    "◀── now ─────────────────────────────────────────────────")))
          (tags-todo "+important-notoday"
                     ((org-agenda-overriding-header
                       "These are your IMPORTANT Tasks")
                      (org-agenda-dim-blocked-tasks)
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
                    "These are your URGENT Tasks")
                   (org-agenda-entry-types '(:deadline))
                   (org-deadline-warning-days 2)
                   (org-agenda-sorting-strategy
                    '(habit-down priority-down timestamp-down))))
          (tags-todo "+joy-notoday"
                     ((org-agenda-overriding-header
                       "These tasks bring JOY")
                      (org-agenda-dim-blocked-tasks)))
          (tags-todo "+notoday"
                     ((org-agenda-overriding-header
                       "I will NOT DO these today")
                      (org-agenda-dim-blocked-tasks)))))
        ("i" "Your IMPORTANT Tasks"
         ((tags-todo "+important-notoday"
                     ((org-agenda-overriding-header
                       "These are your IMPORTANT Tasks")
                      (org-agenda-dim-blocked-tasks)))))
        ("u" "Your URGENT Tasks"
         ((agenda ""
                  ((org-agenda-overriding-header
                    "These are your URGENT Tasks")
                   (org-agenda-entry-types '(:deadline))
                   (org-deadline-warning-days 2)
                   (org-agenda-sorting-strategy
                    '(habit-down priority-down timestamp-down))))))
        ("n" "Your NEXT Tasks" tags-todo "+next")
        ("h" "Your Habits" tags-todo "STYLE=\"habit\"")
        ("r" "Refile" tags "+refile")))
;;; Don't recalculate agenda unless I explicitly say so.
(setq org-agenda-sticky t
      ;; Show me only the Agenda window when I ask for Agenda
      org-agenda-window-setup 'only-window
      ;; Restore the previous window configuration when I'm done
      org-agenda-restore-windows-after-quit t)

;;; Show the Habit graph in all Agenda buffers (where Habits are
;;; available). From:
;;; https://emacs.stackexchange.com/a/17328/20448
(require 'org-habit)
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
  "Any task (without DEADLINE) that can inherit a DEADLINE."
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


(defun fc/org-agenda-inherit-deadline (&optional _arg)
  "Inherit a deadline in agenda."
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
        (org-fold-show-context 'agenda)
        (org-fold-show-entry)
        (org-cycle-hide-drawers 'children)
        (fc/org-inherit-deadline)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))


;; Always highlight current agenda line
;; (add-hook 'org-agenda-mode-hook #'(lambda () (hl-line-mode 1)))

(setq org-agenda-show-all-dates nil
      org-agenda-start-on-weekday 1
      org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
      org-agenda-current-time-string
      "◀── now ─────────────────────────────────────────────────"
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
      "%50ITEM(Task) %5Effort(Effort){:} %5CLOCKSUM %3PRIORITY %20CLOSED %20DEADLINE %20SCHEDULED %20TIMESTAMP %TODO %CATEGORY(Category) %TAGS"
      org-global-properties
      '(("Effort_ALL" . "0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00")
        ("STYLE_ALL" . "habit"))
      org-agenda-clock-consistency-checks
      '(:max-duration "4:00" :min-duration 0 :max-gap 0 :gap-ok-around ("4:00"))
      org-agenda-tags-column 0
      org-agenda-block-separator ?─)

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
(require 'ox-latex)
(require 'ox-confluence)
(require 'ox-taskjuggler)
(dolist (b (list 'beamer 'md 'confluence 'taskjuggler))
  (add-to-list 'org-export-backends b))

;;; If Pygments is installed, use it to export code blocks. Settings
;;; copied from https://stackoverflow.com/a/21007117/137430
(when (executable-find "pygmentize")
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted" nil))
  (setq org-latex-listings 'minted))
(setq org-latex-reference-command "\\cref{%s}"
      org-latex-tables-booktabs t
      org-latex-toc-command "\\tableofcontents \\clearpage"
      org-latex-compiler "lualatex"
      org-latex-hyperref-template
      "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 linktoc=all,
 colorlinks=true,
 linkcolor=blue,
 urlcolor=blue,
 citecolor=blue,
 pdfborder={0 0 1}
 }
"
      org-latex-pdf-process
      '("latexmk -f -pdf -%latex --jobname=%b  -file-line-error --synctex=1 -shell-escape -interaction=nonstopmode -output-directory=%o %f")
      org-image-actual-width nil)
(add-to-list 'org-latex-packages-alist '("capitalize" "cleveref" nil))
(add-to-list 'org-latex-packages-alist '("" "booktabs" nil))
(add-to-list 'org-latex-packages-alist '("" "svg" nil))
(add-to-list 'org-latex-packages-alist '("" "fontspec" nil))
(add-to-list 'org-latex-default-packages-alist
             "\\PassOptionsToPackage{hyphens}{url}")


;; Add Babel execution support for es-mode, if it has been installed.
(eval-after-load 'ob-elasticsearch
  '(progn (org-babel-do-load-languages
           'org-babel-load-languages
           '((elasticsearch . t)))))

;; Add Babel execution support for my languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)
   (plantuml . t)
   (clojure . t)
   (latex . t)))

;;; Use the incredible speed commands introduced in Org 9.4 if they
;;; are available
(setq org-use-speed-commands t
      org-loop-over-headlines-in-active-region 'start-level)

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
                        (if (cl-oddp depth)
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

;; A big thanks to Bernt Hansen for providing an awesome guide to
;; beginners so that we can harness the power of org-mode. Almost all of the
;; customization here, and my complete day-to-day workflow,
;; is based on his document about org-mode which can be
;; found here: http://doc.norang.ca/org-mode.html

(provide 'org-mode-crate)
;;; org-mode-crate.el ends here
