;;; dwin.el --- Navigate and arrange desktop windows   -*- lexical-binding: t; -*-
;;
;; Author: Lars Schmidt-Thieme <schmidt-thieme@ismll.de>
;; Maintainer: Lars Schmidt-Thieme <schmidt-thieme@ismll.de>
;; Version: 0.1
;; Package-Requires: ((emacs "30.2"))
;; Keywords: frames, processes, convenience
;; URL: https://github.com/lsth/dwin
;;
;;; Commentary:
;;
;; Provides two types of navigation:
;; 1. 🏷️ navigation by name,
;;    e.g., switching to firefox, zotero, back to emacs, and
;; 2. 🔀 directional navigation,
;;    e.g., moving to the window to the right or below, as well as
;; 3. ⧉ arranging desktop windows from within emacs with keys.
;;
;; That navigation works globally, also outside emacs, requires that
;; the window manager forwards some keys globally to emacs;
;; for KDE one can bind a one-line script that uses emacsclient
;; to forward the key to emacs, using `dwin-input-key' defined in sect. 1.
;; See etc/bin/dwin-firefox for an example. See Further Details / 1
;; for why we do not use tools like qdotool for sending keys.
;;
;; Emacs then can use functions provided by the window manager
;; to implement window navigation globally. We captured all required
;; methods in a window manager proxy object `dwin-proxy' whose
;; methods can be called via `dwin-call'. The proxy has to be created
;; once before use, e.g., during emacs initialization, using
;; `dwin-setup'. Currently only a proxy for KDE is implemented.
;; But it should be possible to implement further ones.
;; The KDE proxy uses
;; i) dbus calls to org.kde.kglobalaccel (KDE's shortcuts application),
;;    esp. for directional navigation, and
;; ii) kdotool for navigation by name.
;; See sect. 2 below.
;;
;; 🏷️ Navigation by name is provided by `dwin-switch-to-app' that will
;; i) start an app, if it has no window yet,
;; ii) switch to its window, if it is not active, or
;; iii) switch back to emacs, if it is already active (toggle).
;; For each app (firefox, zotero etc.) one needs to
;; i) define one emacs command (e.g., `my/firefox'),
;; ii) bind it to a key in emacs and
;; iii) make sure this key is forwarded globally to emacs,
;;   i.e.,
;;   a) create a small script (e.g., etc/bin/dwin-firefox) and
;;   b) bind it globally to this key.
;; Switching back to emacs is handled by `dwin-switch-to-emacs-or'.
;; It needs the same handling as the other apps above.
;; By default, navigation by name will switch to the first window of an application,
;; if it has several. You can use a prefix arg to switch to a specific one,
;; e.g., C-2 M-x my/firefox or C-2 <f11> to switch to the second one. 
;; See sect. 3 below.
;;
;; For 🔀 directional navigation, we defined a short function
;; `dwin-windmove-left' for each direction. The function tries to
;; move inside emacs via windmove, and if this fails, uses the
;; window manager to move out of emacs.
;; The same method also uses the window manager to move
;; directional from desktop windows. See sect. 4.
;;
;; Sect. 5 contains function `dwin-grab' to ⧉ arrange desktop windows, i.e.,
;; to resize them, reposition them etc. 
;;
;; See etc/example-emacs-init/init.el for an example configuration.
;;
;; Known Issues and Limitations:
;; 1. Requesting help for a **global** key binding with `describe key' will not work.
;;    Emacs will get the key forwarded, but not as input for the already running
;;    describe-key function. In effect, if you say M-x describe-key and then type
;;    a global key like M-left, its action is executed and help for the **next**
;;    key the user types is shown. To view the help, you have to know how the key
;;    is called in emacs and say M-: (describe-key (kbd "M-<left>")) instead.
;;
;;    One could instrument `dwin-input-key' to detect if describe-key is running
;;    by checking (describe-key (kbd "M-<left>")) and then run describe-key with
;;    the key instead of the command the key is bound to. However, I found no
;;    way to cancel the already running describe-key command. So the second issue
;;    will persist. 
;;
;; Further details:
;; 1. Why cannot we just use ydotool to send keys to emacs?
;;    Tools like ydotool seem to be able to send key events only to the
;;    active/focused window and would be able only to implement the cases
;;    where one wants to move from within emacs, not the cases where
;;    one wants to move back to emacs from other applications (or
;;    between other applications). One could reimplement dwin in
;;    bash, then there is no need to input keys into emacs anymore.
;; 
;;    Unfortunately sending dbus events to kwin from bash was unstable
;;    for me: running
;;      qdbus6 org.kde.kglobalaccel /component/kwin org.kde.kglobalaccel.Component invokeShortcut "Switch Window Left"
;;    often (not always) yielded
;;      Cannot find 'org.kde.kglobalaccel.Component' in object /component/kwin at org.kde.kglobalaccel
;;    This never happened when sending the same events from emacs.

;;; Code:
;;_ 0a. requirements
(require 'dbus)

;;_ 0b. some customization

(defgroup dwin nil
  "Customization options for the desktop window manager (dwin) package."
  :group 'applications  ;; optional: parent group
  :prefix "dwin-")      ;; optional: used for variables/functions

(defcustom dwin-switch-to-emacs-function 'switch-to-buffer
  "function to call in `dwin-switch-to-emacs-or' when we are already in emacs."
  :type 'function
  :group 'dwin)

(defcustom dwin-move-x 100
  "default positions to move a window horizonally."
  :type 'integer
  :group 'dwin)

(defcustom dwin-move-y 100
  "default positions to move a window vertically."
  :type 'integer
  :group 'dwin)

(defcustom dwin-resize-x 100
  "default positions to resize a window horizontally."
  :type 'integer
  :group 'dwin)

(defcustom dwin-resize-y 100
  "default positions to resize a window vertically."
  :type 'integer
  :group 'dwin)

(defcustom dwin-be-quiet nil
  "Set to t, if dwin should send fewer messages to the user."
  :type 'boolean
  :group 'dwin)


(defun dwin-message (format &rest args)
  "send a `message', unless `dwin-be-quiet' is true."
  (unless dwin-be-quiet
    (apply #'message format args)))

;;_ 1. input keys programatically (e.g., from emacsclient)
;;;###autoload
(defun dwin-input-key (key)
  "input KEY as if typed by the user.

  Useful to send keys from outside emacs via emacsclient.
  KEY is a string that `kbd' can understand.
  Does only work for KEYs bound to commands, not for prefix keys.
  Uses and consumes a prefix arg currently typed.
  When using commands that modify buffers from emacsclient,
  you need to set a current-buffer, e.g.,
    emacsclient -e \\'(with-current-buffer \"*scratch*\" (dwin-input-key \"a\"))\\'

  Example: (dwin-input-key \"C-<f11>\")."
  (interactive (list
		(read-from-minibuffer "Key to input: ")))
  (if (eq this-command 'describe-key)
      ;; 1. the user is trying to get help for the key: show it
      (progn
	(describe-key (kbd key))
	(message "describe-help still active. Say C-g to cancel."))
    ;; 2. otherwise: execute the command bound to the key
    (let* ((key-vector (read-kbd-macro key))
	   (last-command-event (aref key-vector (- (length key-vector) 1))) ;; then self-insert-command will work
	   (cmd (key-binding (kbd key))))
      (if cmd
	  ;; `command-execute' uses prefix arg, `call-interactively' does not !
	  (command-execute cmd nil key-vector)
	(message "no key binding for %s." key) ))))

;;_ 2. window manager proxy, here for KDE/KWin
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
             ;; List names on the session bus and check for org.kde.KWin
             (member "org.kde.KWin"
                     (dbus-call-method :session
                                       "org.freedesktop.DBus"
                                       "/org/freedesktop/DBus"
                                       "org.freedesktop.DBus"
                                       "ListNames"))
           (error nil)))
    "kwin")
   ;; fallback
   (t "unknown")))

(defun dwin-setup--kwin ()
  "Return a KWin proxy object with directional window-switching methods."
  (let ((self nil))  ;; forward reference for self
    (setq self
          `(;; private methods:
	    (invoke-shortcut . ,(lambda (name)
                                  (condition-case err
                                      (dbus-call-method
                                       :session
                                       "org.kde.kglobalaccel"
                                       "/component/kwin"
                                       "org.kde.kglobalaccel.Component"
                                       "invokeShortcut"
                                       name)
                                    (error (message "KWin D-Bus error: %s" err)))))
            ;; (kdotool . ,(lambda (cmd)
            ;;               (condition-case err
            ;;                   (string-trim (shell-command-to-string (format "kdotool %s" cmd)))
            ;;                 (error (message "kdotool error: %s" err)))))
	    ;; yields a list of lines
            (kdotool . ,(lambda (&rest args)
			  ;; (message "args=%s" (prin1-to-string args))
                          (condition-case err
                              (apply #'process-lines "kdotool" args)
                            (error (message "kdotool error: %s" err)))))
	    ;; public methods:
            (switch-left  . ,(lambda () (dwin-call self 'invoke-shortcut "Switch Window Left")))
            (switch-right . ,(lambda () (dwin-call self 'invoke-shortcut "Switch Window Right")))
            (switch-up    . ,(lambda () (dwin-call self 'invoke-shortcut "Switch Window Up")))
            (switch-down  . ,(lambda () (dwin-call self 'invoke-shortcut "Switch Window Down")))
            (getactivewindow . ,(lambda ()
				  (let ((wins (dwin-call self 'kdotool "getactivewindow")))
				    (when wins
				      (nth 0 wins))))) ; there is always only one active window.
            (search . ,(lambda (&rest query-args) (dwin-call self 'kdotool
							   "search" query-args)))
            (search-class . ,(lambda (class) (dwin-call self 'kdotool
						      "search" "--class" class)))
	    ;; the "" is odd in the call below, but required.
            (search-pid . ,(lambda (pid) (dwin-call self 'kdotool
						  "search" "--all" "--pid" (format "%s" pid) "")))
            (getwindowname . ,(lambda (id) (dwin-call self 'kdotool "getwindowname" id)))
            (getwindowgeometry . ,(lambda (id)
				    (let ((lines (dwin-call self 'kdotool "getwindowgeometry" id)))
				      (mapcar #'string-to-number
					      (append
					       (split-string (string-trim
							      (replace-regexp-in-string "Position: " "" (nth 1 lines))) "," t)
					       (split-string (string-trim
							      (replace-regexp-in-string "Geometry: " "" (nth 2 lines))) "x" t))))))
            (windowactivate . ,(lambda (id) (dwin-call self 'kdotool "windowactivate" id)))
            (windowclose . ,(lambda (id) (dwin-call self 'kdotool "windowclose" id)))
            (windowminimize . ,(lambda (id) (dwin-call self 'kdotool "windowminimize" id)))
            (windowraise . ,(lambda (id) (dwin-call self 'kdotool "windowraise" id)))
            (windowsize . ,(lambda (id width height) (dwin-call self 'kdotool "windowsize" id
							      (format "%s" width) (format "%s" height))))
            (windowmove . ,(lambda (id x y &optional relative)
			     (if relative
				 (dwin-call self 'kdotool "windowmove" "--relative" id
					  (format "%s" x) (format "%s" y))
			       (dwin-call self 'kdotool "windowmove" id
					(format "%s" x) (format "%s" y)))))
            ;; (windowstate . ,(lambda (id) (dwin-call self 'kdotool "windowstate" id))) ; --add/remove/toggle <property>
            (set_desktop_for_window . ,(lambda (id number)
					 (dwin-call self 'kdotool "set_desktop_for_window"
						  (format "%s" id) (format "%s" number))))
            (set_desktop . ,(lambda (number &optional relative)
			      (if relative
				  (dwin-call self 'kdotool "set_desktop" "--relative" (format "%s" number))
				(dwin-call self 'kdotool "set_desktop" (format "%s" number))))) ))
    self))

(defvar dwin-proxy nil
  "proxy object for the current window manager.")

;;;###autoload
(defun dwin-setup ()
  "Setup the desktop window manager dwin.
Create a proxy object to interact with the current window manager.
The object is stored in `dwin-proxy'."
  (let ((wm (dwin-get-window-manager)))
    (pcase wm
      ("kwin" (setq dwin-proxy (dwin-setup--kwin)))
      (_ (progn
	   (setq dwin-proxy nil)
	   (message "no proxy for window manager '%s'." wm) )))))

(defun dwin-call (obj method &rest args)
  "Call METHOD (a symbol) on OBJ (an alist object),
passing ARGS to the method function."
  (let ((fn (cdr (assoc method obj))))
    (if fn
        (apply fn args)
      (error "Method '%s' not found in obj '%s'." method obj))))

(defun dwin-method (obj method)
  "Get METHOD (a symbol) on OBJ (an alist object) as function."
  (cdr (assoc method obj)))


(defun dwin-switch-to-desktop (number &optional relative)
  "switch to desktop NUMBER."
  (dwin-call dwin-proxy 'set_desktop number relative))


;;_ 3. navigation by name
(defvar dwin-process-per-app (make-hash-table :test 'equal)
  "keep track of the process of an app we started (vs. processes started
outside emacs). Used by `dwin-switch-to-app'.")

(defvar dwin-last-window-per-app (make-hash-table :test 'equal)
  "keep track of the window the user has explicitly selected last time.
Used by `dwin-switch-to-app'.")

(defun dwin-reset-window-memory ()
  "reset windows remembered for last app started and last window requested."
  (setq dwin-process-per-app (make-hash-table :test 'equal))
  (setq dwin-last-window-per-app (make-hash-table :test 'equal)))

(defun dwin-buffer-first-line (&optional buf)
  "get the first line of buffer BUF, w/o properties."
  (with-current-buffer (or buf (current-buffer))
      (save-excursion
	(goto-char (point-min))       
	(buffer-substring-no-properties
	 (point)
	 (line-end-position)))) )

(defun dwin-buffer-link (buf-or-name &optional name)
  "return a buffer link to BUF-OR-NAME.
Can be used in messages."
  (let* ((buf (get-buffer buf-or-name))
	 (link-text (or name (buffer-name buf)))) 
    (propertize link-text
		'mouse-face 'highlight
		'help-echo "mouse-1: switch"
		'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [mouse-1] (lambda () (interactive) (switch-to-buffer buf)))
                             map))))

(defun dwin-message-with-link (link &rest args)
  "message the user. Add (propertized) LINK in the *messages* buffer at the end."
  (apply 'message args)
  (with-current-buffer "*Messages*"
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert link "\n"))))
  

;;;###autoload
(defun dwin-run (cmd)
  "launch application/shell command.
Command name or arguments with spaces have to be quoted with \"\",
like in a shell.
Keeps track of the last application launched in `dwin-process-per-app'."
  (interactive (list (read-shell-command "$ ")))
  (let* ((buf-name-output (generate-new-buffer-name (concat "* " cmd " -- output *")))
	 (proc (start-process-shell-command cmd buf-name-output cmd)))
    (dwin-message "dwin-run proc=%s" proc)
    ;; check if it dies immediately
    (sleep-for 0.1) ;; let the process start
    (if (and (not (process-live-p proc)) (not (eq (process-exit-status proc) 0)))
	(dwin-message-with-link
	 (dwin-buffer-link buf-name-output "[📄output]")
	 "❌ error starting %s: %s"
	 cmd
	 (dwin-buffer-first-line buf-name-output) )
      ;; else remember it:
      (puthash cmd proc dwin-process-per-app)
      proc)))

(defun dwin-window-app (name)
  "return the window of the app NAME started last by us via `dwin-run'."
  (let* ((proc (gethash name dwin-process-per-app nil))
	 (pid (when (process-live-p proc)
		(process-id proc)))
	 (wins (when pid
		 (dwin-call dwin-proxy 'search-pid pid)))
	 (win (when wins
		(nth 0 wins))))
    win))

(defun dwin-expand-tilde-only (path)
  "Expand a leading ~ in PATH, but leave the rest of the path relative."
  (if (string-match-p "^~" path)
      (concat (getenv "HOME") (substring path 1))
    path))

(defun dwin-normalize-shell-command (cmd)
  "normalize a shell command, expanding all ~ to the home directory.
Only the normalized command we then can find with pgrep."
  (let* ((cmd-parts (split-string-and-unquote cmd))
	 (cmd-parts-norm (mapcar #'dwin-expand-tilde-only cmd-parts))
	 (cmd-norm (combine-and-quote-strings cmd-parts-norm)))
    cmd-norm))

(defun dwin-find-pids-for-command (cmd)
  "find all PIDs for a given CMD."
  (let* ((cmd-norm (dwin-normalize-shell-command cmd))
	 (pgrep-cmd (concat "pgrep -f " (combine-and-quote-strings (list cmd-norm))))
	 (pids-text (split-string (shell-command-to-string pgrep-cmd)))
	 (pids (mapcar 'string-to-number pids-text)))
	 pids))

(defun dwin-find-windows-for-command (cmd)
  "find all windows for a given CMD, using pids."
  (let* ((pids (dwin-find-pids-for-command cmd))
	 (wins (mapcan (lambda (pid) (dwin-call dwin-proxy 'search-pid pid)) pids)) )
    wins))

;;;###autoload
(defun dwin-switch-to-app (cmd &optional prefix)
  "launch app CMD, if it has not been started yet.
Otherwise activate its window, if it is not active yet.
Otherwise activate emacs (toggle app, go back to emacs).
If there are more than one window, we will activate:
- the one explicitly requested via the PREFIX argument
  (starting at 1, not 0),
- otherwise the last one explicitly requested
  (remembered in `dwin-last-window-per-app'),
- otherwise the (last) one started within emacs
  (remembered in `dwin-process-per-app'),
- otherwise the first one."
  (interactive
   (list ;; (read-from-minibuffer "switch to app: ")
	 (read-shell-command "switch to app: ")
	 "P"))
  ;; 1. check windows
  (let* (;; (cmd-parts (split-string-and-unquote cmd))
	 ;; (cmd-name (when cmd-parts (nth 0 cmd-parts)))
	 ;; (wins (dwin-call dwin-proxy 'search-class cmd-name)) ;; finds all "evince"s, not just "evince ~/a.pdf"
	 (wins (dwin-find-windows-for-command cmd))            ;; via pgrep: finds only "evince ~/a.pdf"
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
		(dwin-window-app cmd)
		(nth 0 wins))))
      ;; 3. do it
      (cond
       ((not wins)
	(dwin-message "launch %s" cmd)
	(dwin-run cmd) )
       ((not is-active)
	(dwin-message "activate %s" cmd)
	(dwin-call dwin-proxy 'windowactivate win)
	nil  ; return nothing
	)
       (t
	(dwin-message "toggle/reactivate emacs")
	(dwin-switch-to-emacs-or) )))))

;;;###autoload
(defun dwin-switch-to-emacs-or ()
  "switch back to emacs, if emacs currently is not the active window.
   Otherwise call `switch-to-buffer'.
   The later function can be customized in `dwin-switch-to-emacs-function'."
  (interactive)
  (if (not (frame-focus-state))
      (select-frame-set-input-focus (selected-frame))
    (call-interactively dwin-switch-to-emacs-function)))

;;_ 4. directional navigation
(defun dwin-windmove--move (move-emacs wm-cmd)
  "Move to a window in a direction.
First checks for emacs window via function MOVE_EMACS.
Then tries external windows via `dwin-proxy'/WM-CMD."
  (if (frame-focus-state)
      ;; 1. emacs focused: try to move inside first
      (condition-case _
	  (funcall move-emacs)
	(error
	 (dwin-call dwin-proxy wm-cmd)))
    ;; 2. emacs not focused: move from external window directly.
    (dwin-call dwin-proxy wm-cmd)) )

;;;###autoload
(defun dwin-windmove-left ()
  "Move to the window on the left.
First checks for emacs windows via `windmove-left'.
Then tries X windows via `dwin-proxy'/switch-left."
  (interactive)
  (dwin-windmove--move #'windmove-left 'switch-left))

;;;###autoload
(defun dwin-windmove-right ()
  "Move to the window on the right.
First checks for emacs windows via `windmove-right'.
Then tries X windows via `dwin-proxy'/switch-right."
  (interactive)
  (dwin-windmove--move #'windmove-right 'switch-right))

;;;###autoload
(defun dwin-windmove-up ()
  "Move to the window one up.
First checks for emacs windows via `windmove-up'.
Then tries X windows via `dwin-proxy'/switch-up."
  (interactive)
  (dwin-windmove--move #'windmove-up 'switch-up))

;;;###autoload
(defun dwin-windmove-down ()
  "Move to the window one down.
First checks for emacs windows via `windmove-down'.
Then tries X windows via `dwin-proxy'/switch-down."
  (interactive)
  (dwin-windmove--move #'windmove-down 'switch-down))


;;_ 5. arrange windows
(defun dwin-resize (window width height &optional relative)
  "set WINDOW to a new size WIDTH and HEIGHT.
If RELATIVE is t, make the new size relative to the old one."
  (when relative
    (let ((geom (dwin-call dwin-proxy 'getwindowgeometry window)))
      (setq width (+ width (nth 2 geom)))
      (setq height (+ height (nth 3 geom))) ))
  (dwin-call dwin-proxy 'windowsize window width height))

(defun dwin--window-names ()
  "get a list of all windows as (id . name) pairs (cons cells)."
  (let* ((wins (dwin-call dwin-proxy 'search-class ""))
	 (names (mapcar (dwin-method dwin-proxy 'getwindowname) wins))
	 (names-with-ids (cl-mapcar (lambda (a b) (format "%s (%s)" a b)) names wins))
	 (name-id-pairs (cl-mapcar 'cons names-with-ids wins)))
    name-id-pairs))

(defvar dwin--history-window-names nil
  "History of window names picked by the user.
Used in `dwin-grab'.")

(defun dwin-read-window (&optional prompt)
  "let the user select a window in the minibuffer.
Returns a pair/cons (NAME . ID)."
  (let ((window-name-id-pairs (dwin--window-names)))
    (assoc
     (completing-read (or prompt "window: ") window-name-id-pairs nil t nil
		      'dwin--history-window-names)
     window-name-id-pairs)))

;;;###autoload
(defun dwin-grab (&optional window)
  "let the user grab and arrange windows interactively, e.g., move, resize etc."
  (interactive
   (list
    (dwin-read-window "window to act on: ")))
  (let* ((win (cdr window))
	 ;; (name (car window))
	 (commands
          `(("a" . ,(lambda () (dwin-call dwin-proxy 'windowactivate win)))
            ("c" . ,(lambda () (dwin-call dwin-proxy 'windowclose win)))
            ("M" . ,(lambda () (dwin-call dwin-proxy 'windowminimize win)))
            ("r" . ,(lambda () (dwin-call dwin-proxy 'windowraise win)))
	    ("<left>" . ,(lambda () (dwin-call dwin-proxy 'windowmove win (- 0 dwin-move-x) 0 t)))
	    ("<right>" . ,(lambda () (dwin-call dwin-proxy 'windowmove win dwin-move-x 0 t)))
	    ("<up>" . ,(lambda () (dwin-call dwin-proxy 'windowmove win 0 (- 0 dwin-move-y) t)))
	    ("<down>" . ,(lambda () (dwin-call dwin-proxy 'windowmove win 0 dwin-move-y t)))
	    ("-" . ,(lambda () (dwin-resize win (- 0 dwin-resize-x) 0 t)))
	    ("+" . ,(lambda () (dwin-resize win dwin-resize-x 0 t)))
	    ("M--" . ,(lambda () (dwin-resize win 0 (- 0 dwin-resize-y) t)))
	    ("M-+" . ,(lambda () (dwin-resize win 0 dwin-resize-y t)))
	    ("W" . ,(lambda()
		      (progn (setq window (dwin-read-window "window to act on: "))
			     (setq win (cdr window))
			     ;; (setq name (car window))
			     )))
	    ("." . ,(lambda()
		      (let ((wins (dwin-call dwin-proxy 'search-pid (emacs-pid))))
			(when wins
			  (setq win (nth 0 wins))
			  ;; (setq name "emacs")
			  ;; (setq window (cons name . win)) 
			))))
	    ("D" . ,(lambda()
		      (let ((number (read-minibuffer "move window to desktop number: ")))
			(dwin-call dwin-proxy 'set_desktop_for_window win number))))
            ;; ("S" . (lambda () (dwin-call dwin-proxy 'windowstate ,win)))
            ("q" . quit)))) 
    ;; loop until user chooses "q" --- poor man's hydra
    (catch 'quit
      (while t
	(message "window: a activate, c close, M minimize, r raise, left/right/up/down move, +/- resize width, M-+/- resize height, D set desktop, W pick window, . pick self, q quit")

        (let* ((key (read-key))   ;  prompt))    ; (read-char-choice (mapcar 'car commands)))
	       (key-desc (single-key-description key))
               (fn (cdr (assoc key-desc commands))))
          (cond ((eq fn 'quit) ; (eq key ?q) ; (string= key-desc "q")
		 (throw 'quit nil))
		(fn
		 (funcall fn))
		(t
		 nil ; unknown command key: do nothing for the moment
		 )))))))


(provide 'dwin)
;;; dwin.el ends here
