;; dwin/etc/example-emacs-inits/03-dwin-via-package-emacs-before-29.1/init.el      -*- lexical-binding: t; -*-
;; - an example emacs init.el loading dwin via package and melpa.
;;   on somewhat older emacs 28.1 and 28.2.
;; - dwin does not run on even older emacs <28.1.

;; 1. package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package--initialized (package-initialize))
(unless package-archive-contents (package-refresh-contents))

;; 1b. extra packages for older emacs < 29.1
;; use-package -- only needed for the example init.el's
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; 2. dwin
(use-package dwin
  :ensure t
  :init
  (when (version< emacs-version "29.1")
    ;; for forward compatibility (see dwin-compat.el)
    (setf dwin-compat--set-transient-map-ORIG (symbol-function 'set-transient-map))
    (defalias 'keymap-set #'dwin-compat--keymap-set)
    (defalias 'set-transient-map #'dwin-compat--set-transient-map))

  :config
  (dwin-setup)

  ;; a. directional navigation
  (global-set-key (kbd "M-<left>")  #'dwin-windmove-left)
  (global-set-key (kbd "M-<right>") #'dwin-windmove-right)
  (global-set-key (kbd "M-<up>")    #'dwin-windmove-up)
  (global-set-key (kbd "M-<down>")  #'dwin-windmove-down)
  ;; Note: Also configure Alt+Arrow keys in KDE shortcuts

  ;; b. named navigation
  ;; b-1. move back to Emacs
  (global-set-key (kbd "C-<f11>") #'dwin-switch-to-emacs-or)

  ;; b-2. Firefox
  (defun my/firefox (&optional prefix)
    (interactive (list current-prefix-arg))
    (dwin-switch-to-app "firefox" prefix))
  (global-set-key (kbd "<f11>") #'my/firefox)

  ;; b-3. Zotero
  (defun my/zotero (&optional prefix)
    (interactive (list current-prefix-arg))
    (dwin-switch-to-app "zotero" prefix))
  (global-set-key (kbd "M-<f11>") #'my/zotero)

  ;; c. Arrange desktop windows
  ;; Use M-x dwin-grab or define an alias if desired
  ;; (defalias #'win #'dwin-grab)
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
