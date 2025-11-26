;;; dwin.el --- Navigate and arrange desktop windows   -*- lexical-binding: t; -*-
;;
;; Author: Lars Schmidt-Thieme <schmidt-thieme@ismll.de>
;; Maintainer: Lars Schmidt-Thieme <schmidt-thieme@ismll.de>
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1") (compat "30.1.0.1"))
;; Keywords: frames, processes, convenience
;; URL: https://github.com/lsth/dwin
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; Provides two types of navigation:
;; 1. 🏷️ navigation by name,
;;    e.g., switching to firefox, zotero, back to Emacs, and
;; 2. 🔀 directional navigation,
;;    e.g., moving to the window to the right or below, as well as
;; 3. ⧉ arranging desktop windows from within Emacs with keys.
;;
;; That navigation works globally, also outside Emacs, requires that
;; the window manager forwards some keys globally to Emacs;
;; for KDE one can bind a one-line script that uses Emacsclient
;; to forward the key to Emacs, using `dwin-input-key' defined in
;; sect.  3. See etc/bin/dwin-firefox for an example.  See
;; Further Details / 1 for why we do not use tools like qdotool for
;; sending keys.
;;
;; Emacs then can use functions provided by the window manager
;; to implement window navigation globally.  We captured all required
;; methods in a window manager proxy object `dwin-proxy' whose
;; methods can be called via `dwin-call'.  The proxy has to be created
;; once before use, e.g., during Emacs initialization, using
;; `dwin-setup'.  Currently only a proxy for KDE is implemented.
;; But it should be possible to implement further ones.
;; The KDE proxy uses
;; i) dbus calls to org.kde.kglobalaccel (KDE's shortcuts
;;    application), esp.  for directional navigation, and
;; ii) kdotool for navigation by name.
;; See sect. 4 below.
;;
;; 🏷️ Navigation by name is provided by `dwin-switch-to-app' that will
;; i) start an app, if it has no window yet,
;; ii) switch to its window, if it is not active, or
;; iii) switch back to Emacs, if it is already active (toggle).
;; For each app (firefox, zotero etc.) one needs to
;; i) define one Emacs command (e.g., `my/firefox'),
;; ii) bind it to a key in Emacs and
;; iii) make sure this key is forwarded globally to Emacs,
;;   i.e.,
;;   a) create a small script (e.g., etc/bin/dwin-firefox) and
;;   b) bind it globally to this key.
;; Switching back to Emacs is handled by `dwin-switch-to-emacs-or'.
;; It needs the same handling as the other apps above.
;; By default, navigation by name will switch to the first window of
;; an application, if it has several.  You can use a prefix arg to
;; switch to a specific one, e.g., C-2 M-x my/firefox or C-2 <f11> to
;; switch to the second one.  See sect. 5 below.
;;
;; For 🔀 directional navigation, we defined a short function
;; `dwin-windmove-left' for each direction.  The function tries to
;; move inside Emacs via windmove, and if this fails, uses the
;; window manager to move out of Emacs.
;; The same method also uses the window manager to move
;; directional from desktop windows.  See sect.  6.
;;
;; Sect.  7 contains function `dwin-grab' to ⧉ arrange desktop
;; windows, i.e., to resize them, reposition them etc.
;;
;; See etc/example-emacs-init/init.el for an example configuration.
;;
;; Known Issues and Limitations:
;; 1. Requesting help for a **global** key binding with `describe key'
;;    will not work.  Emacs will get the key forwarded, but not as
;;    input for the already running describe-key function.  In effect,
;;    if you say M-x describe-key and then type a global key like
;;    M-left, its action is executed and help for the **next** key the
;;    user types is shown.  To view the help, you have to know how the
;;    key is called in Emacs and say M-: (describe-key (kbd
;;    "M-<left>")) instead.  One could instrument `dwin-input-key' to
;;    detect if describe-key is running by checking
;;    (describe-key (kbd "M-<left>")) and then run describe-key with
;;    the key instead of the command the key is bound to.  However, I
;;    found no way to cancel the already running describe-key
;;    command.  So the second issue will persist.
;;
;; Further details:
;; 1. Why cannot we just use ydotool to send keys to Emacs?
;;    Tools like ydotool seem to be able to send key events only to
;;    the active/focused window and would be able only to implement
;;    the cases where one wants to move from within Emacs, not the
;;    cases where one wants to move back to Emacs from other
;;    applications (or between other applications).  One could
;;    reimplement dwin in bash, then there is no need to input keys
;;    into Emacs anymore.
;; 
;;    Unfortunately sending dbus events to kwin from bash was unstable
;;    for me: running
;;      qdbus6 org.kde.kglobalaccel /component/kwin \
;;        org.kde.kglobalaccel.Component invokeShortcut
;;        "Switch Window Left"
;;    often (not always) yielded
;;      Cannot find 'org.kde.kglobalaccel.Component' in object
;;        /component/kwin at org.kde.kglobalaccel
;;    This never happened when sending the same events from Emacs.

;;; Code:
;;_ 0. required packages
(require 'dbus)
(require 'cl-lib)
(require 'server)

(declare-function keymap-set nil)
(when (version< emacs-version "29.1")  ; 28.2: needs it; 29.1: does not need it
  (require 'dwin-compat))

;;_ 1. some customization
(defgroup dwin nil
  "Customization options for the desktop window manager (dwin) package."
  :group 'applications  ;; optional: parent group
  :prefix "dwin-")      ;; optional: used for variables/functions

(defcustom dwin-switch-to-emacs-function 'switch-to-buffer
  "Function `dwin-switch-to-emacs-or' will call when already in Emacs."
  :type 'function
  :group 'dwin)

(defcustom dwin-move-x 100
  "Default positions to move a window horizonally."
  :type 'integer
  :group 'dwin)

(defcustom dwin-move-y 100
  "Default positions to move a window vertically."
  :type 'integer
  :group 'dwin)

(defcustom dwin-resize-x 100
  "Default positions to resize a window horizontally."
  :type 'integer
  :group 'dwin)

(defcustom dwin-resize-y 100
  "Default positions to resize a window vertically."
  :type 'integer
  :group 'dwin)

(defcustom dwin-kwin-use-shortcuts t
  "If the KDE/KWin proxy should use KWin shortcuts.
If nil, implements the behavior in elisp.
To take effect, the proxy has to be reset via `dwin-setup'."
  :type 'boolean
  :group 'dwin)

(defcustom dwin-app-startup-grace-period 0.3
  "Seconds to wait for an app to start to capture new windows."
  :type 'float
  :group 'dwin)

(defcustom dwin-log-level 1
  "How verbose dwin should send messages to the user.
0 = none, 1 = info, 2 = debug."
  :type '(integer :tag "Log level")
  :group 'dwin)

;;_ 2. some prerequisites / utilities
;;_ 2.1 simple alist objects
(defun dwin-get (obj attr)
  "Get attribute ATTR (a symbol) of OBJ (an alist object)."
  (let ((ref-cons (assoc attr obj)))
    (if ref-cons
        (cdr ref-cons)
      (error "Attribute/method '%s' not found in obj '%s'" attr obj))))

(defun dwin-set (obj attr val)
  "Set attribute ATTR (a symbol) of OBJ (an alist object) to value VAL."
  (let ((ref-cons (assoc attr obj)))
    (if ref-cons
	(setcdr ref-cons val)
      (error "Attribute/method '%s' not found in obj '%s'" attr obj))))

(defun dwin-call (obj method &rest args)
  "Call METHOD (a symbol) on OBJ (an alist object).
ARGS are passed as arguments to the method function."
  (let ((fn (dwin-get obj method)))
    (apply fn args)))

(defun dwin-call-apply (obj method args)
  "Call METHOD (a symbol) on OBJ (an alist object).
ARGS is a list of arguments provided to the function.
This is different from `dwin-call' where args can be
several arguments that are then captured by a list.
See the following example that one cannot implement
with `dwin-call' (without changing lines 1+2), but
with `dwin-call-apply'.
Example:
    (let* ((x (list (cons \='fun (lambda(&rest args) (apply \='+ args)))))
           (values1 (list 1 2 3)))
      (dwin-call-apply x \='fun (append values1 (list 4 5)))) ;; 15"
  (let ((fn (dwin-get obj method)))
    (apply fn args)))

(defun dwin-extend (self extensions)
  "Update object SELF with new attributes and functions EXTENSIONS (an alist).
If an attribute or a method exists, overwrite it (using `dwin-set').
If an attribute or a method does not exist yet, define it (appending to SELF).
This function operates in place and will mutate SELF.
Better than using `dwin-set' directly, because it also will work for new
attributes and functions."
  (dolist (pair extensions)
    (let ((key (car pair))
          (val (cdr pair)))
      (let ((cell (assoc key self)))
        (if cell
	    ;; a. attribute/method exists: update
            (dwin-set self key val)
          ;; b. new attribute/method: append
          (nconc self (list pair))))))
  self)

(defalias 'dwin-method #'dwin-get
 "Alias for `dwin-get'. Obsolete. Use `dwin-get' directly.")
(make-obsolete 'dwin-method 'dwin-get "0.1.2")

;;_ 2.2 logging
(defun dwin-message (level format &rest args)
  "Send a `message' with FORMAT and ARGS.
Skip, if LEVEL exceeds `dwin-log-level'."
  (when (<= level dwin-log-level)
    (apply #'message format args)))

;;_ 2.3 functions on basic data structures
(defun dwin-collect (lst pred)
  "Return a sublist of LST of all elements x for which (PRED x) is t."
  (cl-loop for x in lst
           for r = (funcall pred x)
           when r collect x))

(defun dwin-range (a &optional b step)
  "Return a list of integers in a given range.
If only A is given, return integers from 0 to A-1.
If only A and B are given, return integers from A to B-1.
If A, B, and STEP are given, return integers from A to B-1 in steps of STEP."
  (let* ((start (if b a 0))
	 (end (if b b a))
	 (step (or step 1)))
    (cl-loop for i from start below end by step
             collect i)))

(defun dwin-argmin-index (lst pred)
  "Find the index of the element in LST for which PRED returns the smallest value."
  (cl-loop with best-idx = 0
	   with best-val = (funcall pred (nth 0 lst))
	   for x in (cdr lst)
	   for i from 1
           for val = (funcall pred x)
           when (< val best-val)
	   do (setq best-idx i
		    best-val val)
	   finally return best-idx))

(defun dwin-argmin (lst pred)
  "Find the element in LST for which PRED returns the smallest value."
  (cl-loop with best-elem = (nth 0 lst)
	   with best-val = (funcall pred (nth 0 lst))
	   for x in (cdr lst)
           for val = (funcall pred x)
           when (< val best-val)
	   do (setq best-elem x
		    best-val val)
	   finally return best-elem))

(defun dwin-argmax (lst pred)
  "Find the index of the element in LST for which PRED returns the largest value."
  (dwin-argmin lst (lambda (x) (- (funcall pred x)))))

;;_ 3. input keys programatically (e.g., from emacsclient)
;;;###autoload
(defun dwin-input-key (key)
  "Input KEY as if typed by the user.

  Useful to send keys from outside Emacs via emacsclient.
  KEY is a string that `kbd' can understand.
  Does only work for KEYs bound to commands, not for prefix keys.
  Uses and consumes a prefix arg currently typed.
  When using commands that modify buffers from emacsclient,
  you need to set a `current-buffer', e.g.,
    emacsclient -e \\'(with-current-buffer \"*scratch*\"
      (dwin-input-key \"a\"))\\'

  Example: (dwin-input-key \"C-<f11>\")."
  (interactive (list
		(read-from-minibuffer "Key to input: ")))
  (dwin-message 2 "dwin-input-key: %s" key)
  (if (eq this-command 'describe-key)
      ;; 1. the user is trying to get help for the key: show it
      (progn
	(describe-key (kbd key))
	(message "describe-help still active. Say C-g to cancel."))
    ;; 2. otherwise: execute the command bound to the key
    (let* ((key-vector (read-kbd-macro key))
	   (last-command-event (aref key-vector
				     (- (length key-vector) 1)))
					; then self-insert-command
					; will work
	   (cmd (key-binding (kbd key))))
      (if cmd
	  ;; `command-execute' uses prefix arg, `call-interactively'
	  ;; does not !
	  (command-execute cmd nil key-vector)
	(message "no key binding for %s." key) ))))

;;_ 4. window manager proxy, here for X11-generic and KDE/KWin
;; we define three object constructors for window manager proxies:
;; 1. proxy-dotool-based: to pool all methods common for xdotool and kdotool.
;; 2. proxy-x11-generic: to talk to x11 via xdotool, not to any wm specifically.
;; 3. proxy-kwin: to talk to kwin via kdotool and dbus.

;;_ 4.1 common wm proxy methods for dotool based interaction (xdotool, kdotool)
(defun dwin--next-window-in-direction (proxy direction &optional window)
  "Get the next window from WINDOW in DIRECTION.
DIRECTION can be \='left, \='right, \='up or \='down.
If WINDOW is missing, the active window is taken as starting point.
PROXY is the window manager proxy.
TODO: break ties."
  (dwin-message 2 "next window in direction %s" direction)
  (let* ((win-start (or window (dwin-call proxy 'getactivewindow)))
	 (desktop (dwin-call proxy 'get_desktop_for_window win-start))
	 (wins (remove win-start
		       (dwin-call proxy 'search "--desktop" (format "%s" desktop) "")))
	 (geom-start (dwin-call proxy 'getwindowgeometry win-start))
	 (geoms (mapcar (lambda (win) (dwin-call proxy 'getwindowgeometry win)) wins))
	 ;;
	 (pred-filter (pcase direction
			(`left  (lambda (idx) (< (nth 0 (nth idx geoms)) (nth 0 geom-start))))
			(`right (lambda (idx) (> (nth 0 (nth idx geoms)) (nth 0 geom-start))))
			(`up    (lambda (idx) (< (nth 1 (nth idx geoms)) (nth 1 geom-start))))
			(`down  (lambda (idx) (> (nth 1 (nth idx geoms)) (nth 1 geom-start))))))
	 (pred-score (pcase direction
		       (`left  (lambda (idx) (- (nth 0 (nth idx geoms)))))
		       (`right (lambda (idx) (nth 0 (nth idx geoms))))
		       (`up    (lambda (idx) (- (nth 1 (nth idx geoms)))))
		       (`down  (lambda (idx) (nth 1 (nth idx geoms))))))
	 (indices-qual (dwin-collect (dwin-range (length wins)) pred-filter))
	 (index-best   (when indices-qual (dwin-argmin indices-qual pred-score))) )
    (dwin-message 2 "geom-start: %s" geom-start)
    (dwin-message 2 "geoms: %s" geoms)
    (dwin-message 2 "indices-qual: %s" indices-qual)
    (dwin-message 2 "index-best: %s" index-best)

    (when index-best (nth index-best wins))))

(defun dwin--switch-direction (proxy direction)
  "Switch from active window in DIRECTION using PROXY."
  (dwin-message 2 "dwin--switch-direction: %s" direction)
  (let ((win (dwin--next-window-in-direction proxy direction)))
    (when win
      (dwin-call proxy 'windowactivate win))))

(defun dwin--new-proxy-dotool-based ()
  "Create new (abstract) base object for dotool based proxies."
  (let ((self nil))  ;; forward reference for self
    (setq self
           (list
	    ;; private attributes
	    (cons '_class "proxy-dotool-based")
	    (cons 'dotool-name "xdotool")
	    ;; private methods:
	    ;; yields a list of lines
	  ;; xdotool and kdotool >= 0.2.2 signalsexitcode 1 on empty search results. avoid propagating an error.
	  (cons 'dotool (lambda (&rest args)
			  (let ((exitcode))
                            (condition-case err
				(let*
				    ((lines (apply #'process-lines-handling-status
						   (dwin-get self 'dotool-name)
						   (lambda (status) (setq exitcode status))
						   args)))
				  (if (or (zerop exitcode)
					  (null lines)) ; also OK if no output (empty search results)
				      lines
				    (error (message
					    "%s error: %s\n  exit code %s\n  output: %s"
					    (dwin-get self 'dotool-name)
					    args
					    exitcode
					    (string-join lines)))) )
			      (error (message "%s error: %s\n  %s"
					      (dwin-get self 'dotool-name)
					      args
					      err))))))
            ;; (cons 'dotool (lambda (&rest args)
	    ;; 		    ;; (dwin-message 2 "%s %s" (dwin-get self 'dotool-name) args)
            ;;                 (condition-case err
	    ;; 			(apply #'process-lines
	    ;; 			       (dwin-get self 'dotool-name) args)
	    ;; 		      (error (message "%s error: args %s: %s"
	    ;; 				      (dwin-get self 'dotool-name)
	    ;; 				      (print1-to-string args)
	    ;; 				      err)))))
	    ;; public methods:
            (cons 'switch-left  (lambda () (dwin--switch-direction self 'left)))
            (cons 'switch-right (lambda () (dwin--switch-direction self 'right)))
            (cons 'switch-up    (lambda () (dwin--switch-direction self 'up)))
            (cons 'switch-down  (lambda () (dwin--switch-direction self 'down)))
            (cons 'getactivewindow (lambda ()
				     (let ((wins (dwin-call self
							    'dotool
							    "getactivewindow")))
				       (when wins
					 (nth 0 wins))))) ; there is always only one active window.
            (cons 'get_desktop (lambda ()
				 (let ((output (dwin-call self 'dotool "get_desktop")))
				   (when output
				     (string-to-number (nth 0 output))))))
            (cons 'search (lambda (&rest query-args) (dwin-call-apply
						      self 'dotool
						      (append (list "search") query-args) )))
            (cons 'search-class (lambda (class) (dwin-call self 'dotool
							   "search"
							   "--class"
							   class)))
            (cons 'search-pid (lambda (pid) (dwin-call self 'dotool
						       "search" ;; "--all"
						       "--pid"
						       (format "%s" pid) )))
            (cons 'getwindowname (lambda (id) (nth 0 (dwin-call
						      self 'dotool
						      "getwindowname" id))))
            (cons 'getwindowgeometry (lambda (id)
				       (let ((lines (dwin-call
						     self 'dotool
						     "getwindowgeometry"
						     id)))
					 (mapcar #'string-to-number
						 (append
						  (split-string
						   (string-trim
						    (replace-regexp-in-string
						     "Position: " ""
						     (nth 1 lines)))
						   "," t)
						  (split-string
						   (string-trim
						    (replace-regexp-in-string
						     "Geometry: " ""
						     (nth 2 lines)))
						   "x" t))))))
            (cons 'windowactivate (lambda (id) (dwin-call
						self 'dotool
						"windowactivate" id)))
            (cons 'windowclose (lambda (id) (dwin-call self 'dotool
						       "windowclose" id)))
            (cons 'windowminimize (lambda (id) (dwin-call
						self 'dotool
						"windowminimize" id)))
            (cons 'windowraise (lambda (id) (dwin-call
					     self 'dotool
					     "windowraise" id)))
            (cons 'windowsize (lambda (id width height) (dwin-call
							 self 'dotool
							 "windowsize" id
							 (format "%s" width)
							 (format "%s" height))))
            (cons 'windowmove (lambda (id x y &optional relative)
				(if relative
				    (dwin-call self 'dotool
					       "windowmove" "--relative"
					       id (format "%s" x)
					       (format "%s" y))
				  (dwin-call self 'dotool "windowmove" id
					     (format "%s" x) (format "%s" y)))))
            ;; (cons 'windowstate (lambda (id) (dwin-call self 'dotool
	    ;;   "windowstate" id))) ; --add/remove/toggle <property>
            (cons 'get_desktop_for_window (lambda (id)
					    (string-to-number (nth 0
					     (dwin-call self 'dotool
							"get_desktop_for_window"
							(format "%s" id))))))
            (cons 'set_desktop_for_window (lambda (id number)
					    (dwin-call self 'dotool
						       "set_desktop_for_window"
						       (format "%s" id)
						       (format "%s" number))))
            (cons 'set_desktop (lambda (number &optional relative)
				 (if relative
				     (dwin-call self 'dotool
						"set_desktop" "--relative"
						(format "%s" number))
				   (dwin-call self 'dotool "set_desktop"
					      (format "%s" number)))))
	    (cons 'short-windowid (lambda (id) id)) ))
    self))

;;_ 4.2 wm proxy for X11 generic (xdotool)
(defun dwin-x11--window-type (window property)
"Get X11 PROPERTY of WINDOW."
(let* ((cmd (format "xprop -id %s %s" window property))
       (output (shell-command-to-string cmd))
       (value (when (string-match "\\([^=]+\\) = \\(.*\\)" output)
		(match-string 2 output))))
  value))

(defun dwin-x11--window-normalp (window)
  "Check if X11 WINDOW is of type normal."
  (string= (dwin-x11--window-type window "_NET_WM_WINDOW_TYPE")
	   "_NET_WM_WINDOW_TYPE_NORMAL"))

(defun dwin--new-proxy-x11-generic ()
  "Create new X11 proxy object."
  (let ((self (dwin--new-proxy-dotool-based)))  ;; despite origin from X11, will also work with Wayland/KDE.
    ;; further methods:
    (dwin-extend self
	 (list
	  ;; overwrite attributes (new values)
	  (cons '_class "proxy-x11-generic")
	  ;; private methods
	  ;; xdotool returns x11 windows of all types (normal, tooltip etc.).
	  ;; Usually we need only the normal ones.
	  ;; Filter windows to retain just the normal ones.
	  ;; TODO: when overwriting methods, keep the overwritten ones and call them here.
	  ;; TODO: also add the filtered version of search-class and search.
	  (cons 'search-filter #'dwin-x11--window-normalp)
	  (cons 'search-pid (lambda (pid &optional pred)
			      "Get all X11 windows for given PID that fulfill PRED."
			      (let* ((pred (or pred (dwin-get self 'search-filter)))
				     (wins (dwin-call self 'dotool
						      "search"  ;;  "--all"
						      "--pid"
						      (format "%s" pid) )))
				(dwin-collect wins pred))))
	  (cons 'search-class (lambda (class &optional pred)
				"Get all X11 windows of a given CLASS that fulfill PRED."
				(let* ((pred (or pred (dwin-get self 'search-filter)))
				       (wins (dwin-call self 'dotool
							"search" "--class" class)))
				  (dwin-collect wins pred))))
	  ;; = super.search: (as &rest args and &optional pred cannot be combined)
	  (cons 'search-unfiltered (lambda (&rest query-args)
			  "Get all X11 windows matching query formulated by QUERY-ARGS.
Does yield all windows without applying any filters."
			   (dwin-call-apply self 'dotool
					    (append (list "search") query-args) )))
	  (cons 'search (lambda (&rest query-args)
			  "Get all X11 windows matching query formulated by QUERY-ARGS.
Filters using `search-filter'."
			  (let* ((pred (dwin-get self 'search-filter))
				 (wins (dwin-call-apply self 'search-unfiltered query-args)))
			    (dwin-collect wins pred)))) ))))


;;_ 4.3 wm proxy for KDE/KWin (kdotool)
(defun dwin--new-proxy-kwin ()
  "Create new KWin proxy object with directional window-switching methods."
  (let ((self (dwin--new-proxy-dotool-based)))
    ;; further methods:
    (dwin-extend self
	 (list
	  ;; overwrite attributes (new values)
	  (cons '_class "proxy-kwin")
	  (cons 'dotool-name "kdotool")
	  ;; public methods:
	  ;; "kdotool search --pid <pid>" throws odd error "Error: missing argument for option '--pid'"
	  ;; search for two properties (--all) with an empty condition ("") for the 2nd one works.
          (cons 'search-pid (lambda (pid) (dwin-call self 'dotool
						     "search" "--all"
						     "--pid" (format "%s" pid)
						     "")))
	  (cons 'short-windowid (lambda (id)
				  (substring id 1 5))) ))
    (when dwin-kwin-use-shortcuts
      (dwin-extend self
	 (list
	  ;; private methods:
	  (cons 'invoke-shortcut (lambda (name)
				   (dwin-message 2 "invoke-shortcut: %s" name)
				   (condition-case err
				       (dbus-call-method
					:session
					"org.kde.kglobalaccel"
					"/component/kwin"
					"org.kde.kglobalaccel.Component"
					"invokeShortcut"
					name)
				     (error (message
					     "KWin D-Bus error: %s"
					     err)))))
	  ;; public methods:
	  (cons 'switch-left  (lambda () (dwin-call self
						    'invoke-shortcut
						    "Switch Window Left")))
	  (cons 'switch-right (lambda () (dwin-call self
						    'invoke-shortcut
						    "Switch Window Right")))
	  (cons 'switch-up    (lambda () (dwin-call self
						    'invoke-shortcut
						    "Switch Window Up")))
	  (cons 'switch-down  (lambda () (dwin-call self
						    'invoke-shortcut
						    "Switch Window Down"))) )))
	 self))

;;_ 4.4 setting up the wm proxy
(defvar dwin-proxy nil
  "Proxy object for the current window manager.")

(defun dwin-get-window-manager ()
  "Return the name of the current window manager.
Detects KWin via D-Bus, EXWM if running under Emacs,
otherwise returns \"unknown\"."
  (cond
   ;; EXWM detection
   ((and (featurep 'exwm) (bound-and-true-p exwm--connection))
    "exwm")
   ;; KWin detection via D-Bus (check if service exists)
   ((and (fboundp 'dbus-call-method)
         (condition-case nil
             ;; List names on the session bus and check for
	     ;; org.kde.KWin
             (member "org.kde.KWin"
                     (dbus-call-method :session
                                       "org.freedesktop.DBus"
                                       "/org/freedesktop/DBus"
                                       "org.freedesktop.DBus"
                                       "ListNames"))
           (error nil)))
    "kwin")
   ((string= (getenv "XDG_SESSION_TYPE") "x11")
    "X11-generic")
   ;; fallback
   (t "unknown")))

(defun dwin-setup-proxy ()
  "Setup the desktop window manager dwin.
Create a proxy object to interact with the current window manager.
The object is stored in `dwin-proxy'."
  (let ((wm (dwin-get-window-manager)))
    (pcase wm
      ("kwin" (setq dwin-proxy (dwin--new-proxy-kwin)))
      ("x11-generic" (setq dwin-proxy (dwin--new-proxy-x11-generic)))
      (_ (progn
	   (setq dwin-proxy nil)
	   (message "no proxy for window manager '%s'." wm) )))))

;;;###autoload
(defun dwin-setup ()
  "Setup the desktop window manager dwin.
Create a proxy object via `dwin-setup-proxy' to interact with the current
window manager / compositor and store in `dwin-proxy'.
Also start the Emacs server, if not running already."
  (unless (server-running-p)
    (server-start))
  (dwin-setup-proxy))

;;_ 4.5 convenience functions using the setup wm proxy
(defun dwin-switch-to-desktop (number &optional relative)
  "Switch to desktop NUMBER.
If RELATIVE is t, switch relative to the current desktop."
  (dwin-call dwin-proxy 'set_desktop number relative))


;;_ 5. navigation by name
(defvar dwin-process-per-app (make-hash-table :test 'equal)
  "Keep track of processes of apps started from within Emacs.
This deliberately does not cover processes started outside Emacs.
The key is the cmd used to start the process.
The value is an alist with fields
- process: the process object,
- windows: windows created briefly after startup.
  (i.e., new windows spawned as reaction to a launcher
  app like \='firefox --new-window\='.
Used by `dwin-switch-to-app'.")

(defvar dwin-last-window-per-app (make-hash-table :test 'equal)
  "Keep track of the window the user has explicitly selected last time.
Used by `dwin-switch-to-app'.")

(defun dwin-reset-window-memory ()
  "Reset windows remembered for last app started and last window requested."
  (setq dwin-process-per-app (make-hash-table :test 'equal))
  (setq dwin-last-window-per-app (make-hash-table :test 'equal)))

(defun dwin-buffer-first-line (&optional buf)
  "Get the first line of buffer BUF, w/o properties."
  (with-current-buffer (or buf (current-buffer))
      (save-excursion
	(goto-char (point-min))
	(buffer-substring-no-properties
	 (point)
	 (line-end-position)))) )

(defun dwin-buffer-link (buf-or-name &optional name)
  "Return a buffer link to BUF-OR-NAME.
Use NAME as link text, if given.
Can be used in messages."
  (let* ((buf (get-buffer buf-or-name))
	 (link-text (or name (buffer-name buf))))
    (propertize link-text
		'mouse-face 'highlight
		'help-echo "mouse-1: switch"
		'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [mouse-1]
					 (lambda () (interactive)
					   (switch-to-buffer buf)))
                             map))))

(defun dwin-message-with-link (link &rest args)
  "Message the user, including a (propertized) LINK in the *messages* buffer.
ARGS contain the message format and arguments."
  (apply #'message args)
  (with-current-buffer "*Messages*"
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert link "\n"))))
  

;;;###autoload
(defun dwin-run (cmd)
  "Launch application/shell command CMD.
Command name or arguments with spaces have to be quoted with \"\",
like in a shell.
Keeps track of the last application launched in
`dwin-process-per-app'."
  (interactive (list (read-shell-command "$ ")))
  (let* ((buf-name-output (generate-new-buffer-name
			   (concat "* " cmd " -- output *")))
	 (result nil)
	 (wins-before (dwin-call dwin-proxy 'search-class ""))
	 (proc (start-process-shell-command cmd buf-name-output cmd)))
    (dwin-message 2 "dwin-run proc=%s" proc)
    ;; check if it dies immediately
    (sleep-for dwin-app-startup-grace-period) ;; let the process start
    (cond ((and (not (process-live-p proc))
		(not (eq (process-exit-status proc) 0)))
	   ;; a. process ended with error
	   (let ((error-msg (format "❌ error starting %s: %s"
				    cmd
				    (dwin-buffer-first-line buf-name-output))))
	     (dwin-message-with-link
	      (dwin-buffer-link buf-name-output "[📄output]")
	      error-msg)
	     (setq result (list (cons 'error error-msg))) ))
	  ((not (process-live-p proc))
	   ;; b. process ended w/o error: maybe it created a window? (launcher)
	   (let* ((wins-after (dwin-call dwin-proxy 'search-class ""))
		  (wins-new (cl-set-difference wins-after wins-before :test #'equal)))
	     ;; remember new windows:
	     (setq result (list (cons 'windows wins-new)) )))
	  (t
	   ;; c. process runs: remember process & pid:
	   (setq result (list (cons 'process proc)))))
    (when (not (alist-get 'error result nil))
      ;; do not record errors
      (puthash cmd result dwin-process-per-app))
    result))

(defun dwin-window-for-command-started-by-us (cmd)
  "Return the window of the command CMD started last by us via `dwin-run'."
  (let* ((proc-or-windows (gethash cmd dwin-process-per-app nil))
	 (proc (alist-get 'process proc-or-windows nil))
	 ;; a. windows captured at startup
	 (windows-start (alist-get 'windows proc-or-windows nil))
	 ;; 
	 (pid (when (process-live-p proc)
		(process-id proc)))
	 ;; b. windows associated with the process
	 (windows-pid (when pid
			(dwin-call dwin-proxy 'search-pid pid)))
	 (windows (append windows-pid windows-start))
	 (win (when windows
		(nth 0 windows))) ) ; for now just take the first
    win))

(defun dwin-expand-tilde-only (path)
  "Expand a leading ~ in PATH, but leave the rest of the path relative."
  (if (string-match-p "^~" path)
      (concat (getenv "HOME") (substring path 1))
    path))

(defun dwin-normalize-shell-command (cmd)
  "Normalize a shell command CMD, expanding all ~ to the home directory.
Only the normalized command we then can find with pgrep."
  (let* ((cmd-parts (split-string-and-unquote cmd))
	 (cmd-parts-norm (mapcar #'dwin-expand-tilde-only cmd-parts))
	 (cmd-norm (combine-and-quote-strings cmd-parts-norm)))
    cmd-norm))

(defun dwin-find-pids-for-command (cmd)
  "Find all PIDs for a given CMD."
  (let* ((cmd-norm (dwin-normalize-shell-command cmd))
	 (pgrep-cmd (concat "pgrep -f " (combine-and-quote-strings
					 (list cmd-norm))))
	 (pids-text (split-string (shell-command-to-string pgrep-cmd)))
	 (pids (mapcar #'string-to-number pids-text)))
	 pids))

(defun dwin-find-windows-for-command (cmd)
  "Find all windows for a given CMD.
Check two sources:
- all running processes having CMD as command and their windows.
- windows created when we started CMD."
  (let* ((pids (dwin-find-pids-for-command cmd))
	 (windows-processes (mapcan (lambda (pid) (dwin-call dwin-proxy
							  'search-pid pid))
				 pids))
	 (windows-start (alist-get 'windows
				   (gethash cmd dwin-process-per-app nil) nil))
	 (windows (append windows-processes windows-start)))
    (dwin-message 2 "win/cmd: '%s'\n  @proc: %s\n  @start: %s"
		  cmd windows-processes windows-start)
    windows))

;;;###autoload
(defun dwin-switch-to-app (cmd &optional prefix)
  "Launch app CMD, if it has not been started yet.
Otherwise activate its window, if it is not active yet.
Otherwise activate Emacs (toggle app, go back to Emacs).
If there are more than one window, we will activate:
- the one explicitly requested via the PREFIX argument
  (starting at 1, not 0),
- otherwise the last one explicitly requested
  (remembered in `dwin-last-window-per-app'),
- otherwise the (last) one started within Emacs
  (remembered in `dwin-process-per-app'),
- otherwise the first one."
  (interactive
   (list ;; (read-from-minibuffer "switch to app: ")
	 (read-shell-command "switch to app: ")
	 ;; TODO@2027/11: replace the following line by the modern standard,
	 ;; for backwards compatibility keep the old for now.
	 ;; Also edit README.md and the examples accordingly.
	 ;; "P"))
	 (list current-prefix-arg)))
  ;; 1. check windows
  (let* ((wins (dwin-find-windows-for-command cmd))
	 (active-win (dwin-call dwin-proxy 'getactivewindow))
	 (is-active (member active-win wins))
	 ;;
	 ;; 2. handle the prefix arg
	 (prefix-num (prefix-numeric-value prefix))
	 (win-prefix nil))
    (when (and wins prefix (<= prefix-num (length wins)) (> prefix 0))
      (setq win-prefix (nth (- prefix-num 1) wins))
      (puthash cmd win-prefix dwin-last-window-per-app) )
    (let ((win (or
		win-prefix
		(gethash cmd dwin-last-window-per-app nil)
		(dwin-window-for-command-started-by-us cmd)
		(nth 0 wins))))
      ;; 3. do it
      (cond
       ((not wins)
	(dwin-message 1 "launch %s" cmd)
	(dwin-run cmd) )
       ((not is-active)
	(dwin-message 1 "activate %s" cmd)
	(dwin-call dwin-proxy 'windowactivate win)
	nil  ; return nothing
	)
       (t
	(dwin-message 1 "toggle/reactivate Emacs")
	(dwin-switch-to-emacs-or) )))))

;;;###autoload
(defun dwin-switch-to-emacs-or ()
  "Switch back to Emacs, if Emacs currently is not the active window.
Otherwise call `switch-to-buffer'.
The later function can be customized in
`dwin-switch-to-emacs-function'."
  (interactive)
  (if (not (frame-focus-state))
      (select-frame-set-input-focus (selected-frame))
    (call-interactively dwin-switch-to-emacs-function)))

;;_ 6. directional navigation
(defun dwin-windmove--move (move-emacs wm-cmd)
  "Move to a window in a direction.
First checks for Emacs window via function MOVE-EMACS.
Then tries external windows via `dwin-proxy'/WM-CMD."
  (if (frame-focus-state)
      ;; 1. Emacs focused: try to move inside first
      (condition-case _
	  (funcall move-emacs)
	(error
	 (dwin-call dwin-proxy wm-cmd)))
    ;; 2. Emacs not focused: move from external window directly.
    (dwin-call dwin-proxy wm-cmd)) )

;;;###autoload
(defun dwin-windmove-left ()
  "Move to the window on the left.
First checks for Emacs windows via `windmove-left'.
Then tries X windows via `dwin-proxy'/switch-left."
  (interactive)
  (dwin-windmove--move #'windmove-left 'switch-left))

;;;###autoload
(defun dwin-windmove-right ()
  "Move to the window on the right.
First checks for Emacs windows via `windmove-right'.
Then tries X windows via `dwin-proxy'/switch-right."
  (interactive)
  (dwin-windmove--move #'windmove-right 'switch-right))

;;;###autoload
(defun dwin-windmove-up ()
  "Move to the window one up.
First checks for Emacs windows via `windmove-up'.
Then tries X windows via `dwin-proxy'/switch-up."
  (interactive)
  (dwin-windmove--move #'windmove-up 'switch-up))

;;;###autoload
(defun dwin-windmove-down ()
  "Move to the window one down.
First checks for Emacs windows via `windmove-down'.
Then tries X windows via `dwin-proxy'/switch-down."
  (interactive)
  (dwin-windmove--move #'windmove-down 'switch-down))


;;_ 7. arrange windows
(defun dwin-resize (window width height &optional relative)
  "Set WINDOW to a new size WIDTH and HEIGHT.
If RELATIVE is t, make the new size relative to the old one."
  (when relative
    (let ((geom (dwin-call dwin-proxy 'getwindowgeometry window)))
      (setq width (+ width (nth 2 geom)))
      (setq height (+ height (nth 3 geom))) ))
  (dwin-call dwin-proxy 'windowsize window width height))

(defun dwin--window-names ()
  "Get a list of all windows as (id . name) pairs (cons cells)."
  (let* ((wins (dwin-call dwin-proxy 'search-class ""))
	 (names (mapcar (dwin-get dwin-proxy 'getwindowname) wins))
	 (names-with-ids (cl-mapcar (lambda (a b)
				      (format "%s (%s)" a
					      (dwin-call dwin-proxy 'short-windowid b)))
					      ;; b))
				    names wins))
	 (name-id-pairs (cl-mapcar 'cons names-with-ids wins)))
    name-id-pairs))

;; (dwin-setup)
;; (dwin--window-names)
;; (dwin-call dwin-proxy 'getwindowname "{0b82ec55-ef36-4a0b-8de6-5c201ac0039f}")
;; (dwin-call dwin-proxy 'short-windowid "{0b82ec55-ef36-4a0b-8de6-5c201ac0039f}")

(defvar dwin--history-window-names nil
  "History of window names picked by the user.
Used in `dwin-grab'.")

(defun dwin-read-window (&optional prompt)
  "Let the user select a window in the minibuffer.
Show PROMPT.
Returns a pair/cons (NAME . ID)."
  (let ((window-name-id-pairs (dwin--window-names)))
    (assoc
     (completing-read (or prompt "window: ") window-name-id-pairs
		      nil t nil 'dwin--history-window-names)
     window-name-id-pairs)))

(defun dwin-current-window-or-ask (prompt)
  "Return the `dwin-current-window', if bound and set; otherwise ask the user.
PROMPT gives a short text to use when asking.
Uses `dwin-read-window' to ask the user.
`dwin-current-window' should be set only locally."
  (if (and (boundp 'dwin-current-window) dwin-current-window)
      dwin-current-window
    (dwin-read-window prompt)))

;; short commands with some doc, so we can bind them to keys
(defun dwin-select (window)
  "Select WINDOW as `dwin-current-window'."
  (interactive (list (dwin-read-window "window: ")))
  (setq-local dwin-current-window (cdr window)))

(defun dwin-select-emacs ()
  "Select Emacs' own window as `dwin-current-window'."
  (interactive)
  (let ((wins (dwin-call dwin-proxy 'search-pid (emacs-pid))))
    (when wins
      (setq-local dwin-current-window (nth 0 wins)))))

;; (setq dwin-current-window nil)  (setq win nil) window

(defun dwin-activate (window)
  "Activate WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowactivate window))

(defun dwin-raise (window)
  "Raise WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowraise window))

(defun dwin-close (window)
  "Close WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowclose window))

(defun dwin-minimize (window)
  "Minimize WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowminimize window))

(defun dwin-move-left (window)
  "Move WINDOW to the left.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowmove window (- dwin-move-x) 0 t))

(defun dwin-move-right (window)
  "Move WINDOW to the right.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowmove window dwin-move-x 0 t))

(defun dwin-move-up (window)
  "Move WINDOW up.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowmove window 0 (- dwin-move-y) t))

(defun dwin-move-down (window)
  "Move WINDOW down.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-call dwin-proxy 'windowmove window 0 dwin-move-y t))

(defun dwin-resize-decrease-width (window)
  "Decrease width of WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-resize window (- dwin-resize-x) 0 t))

(defun dwin-resize-increase-width (window)
  "Increase width of WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-resize window dwin-resize-x 0 t))

(defun dwin-resize-decrease-height (window)
  "Decrease height of WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-resize window 0 (- dwin-resize-y) t))

(defun dwin-resize-increase-height (window)
  "Increase height of WINDOW.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list (dwin-current-window-or-ask "window: ")))
  (dwin-resize window 0 dwin-resize-y t))

(defun dwin-move-to-desktop (window desktop)
  "Move WINDOW to DESKTOP.
If missing, take `dwin-current-window', else ask the user."
  (interactive (list
		(dwin-current-window-or-ask "window: ")
		(read-minibuffer "move window to desktop number: ")))
  (dwin-call dwin-proxy 'set_desktop_for_window window desktop))

(defvar dwin-arrange-keymap
  (let ((map (make-sparse-keymap)))
    ;; choose window
    (keymap-set map "W" #'dwin-select)
    (keymap-set map "." #'dwin-select-emacs)
    ;; visibility
    (keymap-set map "a" #'dwin-activate)
    (keymap-set map "c" #'dwin-close)
    (keymap-set map "M" #'dwin-minimize)
    (keymap-set map "r" #'dwin-raise)
    ;; move
    (keymap-set map "<left>"  #'dwin-move-left)
    (keymap-set map "<right>" #'dwin-move-right)
    (keymap-set map "<up>"    #'dwin-move-up)
    (keymap-set map "<down>"  #'dwin-move-down)
    (keymap-set map "D"       #'dwin-move-to-desktop)
    ;; resize
    (keymap-set map "-"   #'dwin-resize-decrease-width)
    (keymap-set map "+"   #'dwin-resize-increase-width)
    (keymap-set map "M--" #'dwin-resize-decrease-height)
    (keymap-set map "M-+" #'dwin-resize-increase-height)
    ;; quit
    (keymap-set map "q" 'ignore) ; quit
    map)
  "Keymap for dwin arrange commands.")

(defun dwin-grab (window)
  "Arrange WINDOW interactively using `dwin-arrange-keymap'."
  (interactive
   (list (cdr (dwin-read-window "window to arrange: "))))
  (save-excursion
    (switch-to-minibuffer)
    (setq-local dwin-current-window window)
    (set-transient-map
     dwin-arrange-keymap
     (lambda () (not (eq this-command 'ignore)))
     (lambda () (message "arranging windows done."))
     "arrange window %k: ")))

(provide 'dwin)
;;; dwin.el ends here
