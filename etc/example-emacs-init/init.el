;; dwin/etc/example-emacs-config/init.el      -*- lexical-binding: t; -*-
;; - an example emacs init.el loading dwin.

;;_ 1. straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;_ 2. emacs server  (required to get global keys when emacs has no focus)
(require 'server)
(unless (server-running-p)
  (server-start))

;;_ 3. dwin 
(use-package dwin
  :straight (dwin :type git :repo "https://github.com/lsth/dwin.git")
  :config
  (dwin-setup)
  ;; a. directional navigation
  (global-set-key (kbd "M-<left>") #'dwin-windmove-left)
  (global-set-key (kbd "M-<right>") #'dwin-windmove-right)
  (global-set-key (kbd "M-<up>") #'dwin-windmove-up)
  (global-set-key (kbd "M-<down>") #'dwin-windmove-down)
  ;; do not forget: also set alt-left/right/up/down in kde shortcuts (so we can move back).

  ;; b. named navigation
  ;; b-1. move back to emacs
  (global-set-key (kbd "C-<f11>") #'dwin-switch-to-emacs-or)

  ;; b-2. firefox
  (defun my/firefox (&optional prefix)
    (interactive "P")
    (dwin-switch-to-app "firefox" prefix))
  (global-set-key (kbd "<f11>") #'my/firefox)

  ;; b.3 zotero
  (defun my/zotero (&optional prefix)
    (interactive "P")
    (dwin-switch-to-app "zotero" prefix))
  (global-set-key (kbd "M-<f11>") #'my/zotero)
  ;; do not forget: also set keys in kde shortcuts (so we can move back).

  ;; c. arrange desktop windows
  ;; just use M-x dwin-grab. You can alias it to something shorter, e.g.
  ;; (defalias #'win #'dwin-grab)
)
