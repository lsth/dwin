;;; dwin-compat.el --- Backward ports of functions for older E_m_a_c_s  -*- lexical-binding: t; -*-
;;
;; Author: Lars Schmidt-Thieme <schmidt-thieme@ismll.de>
;; Keywords: frames, processes, convenience
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1") (compat "30.1.0.1"))
;; URL: https://github.com/lsth/dwin
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;; A few functions used by dwin that do not exist or exist in different from
;; in older Emacs, and that are easy to provide. These functions seem not to
;; be covered by compat.
;;
;; Emacs 27.2 misses `process-lines-handling-status' (and maybe more, I did
;; not check). I just put 28.1 as minimum version for now.

;;; Code:

;;_ 1. substitute functions
(defun dwin-compat--keymap-set (map key fun)
  "Dwin compatibility drop in via `define-key'.
Covers all arguments MAP, KEY, FUN."
  (define-key map (kbd key) fun))

(defvar dwin-compat--set-transient-map-ORIG nil
  "Dwin compatibility drop in.")

(defun dwin-compat--set-transient-map (map &optional keep-pred on-exit message timeout)
  "Dwin compatibility drop in via `dwin-compat--set-transient-map-ORIG'.
We pass on MAP, KEEP-PRED, ON-EXIT.
We ignore MESSAGE, TIMEOUT."
  ;; for now we just drop MESSAGE and TIMEOUT.
  ;; TODO: find a way to show the message. TIMEOUT we do not use anyway.
  (ignore message)
  (ignore timeout)
  (funcall dwin-compat--set-transient-map-ORIG map keep-pred on-exit))

;;_ 2. semantically conditioned subtitutions
;; aliasing functions outside dwin namespace not allowed by melpazoid.
;; shift them to the init file.
;; (when (not (fboundp 'keymap-set))
;;     ;; emacs < 29.1
;;     (defalias 'keymap-set #'dwin-compat--keymap-set))

;;_ 3. mere version-conditioned subtitutions
;; (when (version< emacs-version "29.1")
;;   (setf dwin-compat--set-transient-map-ORIG (symbol-function 'set-transient-map))
;;   (defalias 'set-transient-map #'dwin-compat--set-transient-map))

(provide 'dwin-compat)
;;; dwin-compat.el ends here
