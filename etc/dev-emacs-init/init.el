;; etc/dev-emacs-init/init.el  -*- lexical-binding: t; -*-
(require 'package)

;; Add GNU and MELPA
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; Only refresh contents if empty
(unless package-archive-contents
  (package-refresh-contents))

;; Install dev packages if missing
(dolist (pkg '(package-lint))
  (unless (package-installed-p pkg)
    (message "Installing %s..." pkg)
    (package-install pkg)))

;; Load package-lint
(require 'package-lint)
;; (require 'package-lint-batch)

;; Add repo root and tests to load-path
(add-to-list 'load-path (expand-file-name "../../" (file-name-directory load-file-name)))  ;; repo root
(add-to-list 'load-path (expand-file-name "../tests" (file-name-directory load-file-name))) ;; tests folder
