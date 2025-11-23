<h1 align="center">Emacs Desktop Window Manager (dwin)</h1>
<h2 align="center">Manage desktop windows from your Emacs</h2>
<p align="center">
  <a href="#setup"><b>Setup 🏁</b></a> |
  <a href="#usage"><b>Advanced Usage 🚀</b></a> |
  <a href="#impl"><b>Implementation 🛠️</b></a> |
  <a href="#relatedwork"><b>Related Work 📚</b></a>
</p>
<hr>

<p align="justify">
Move seamlessly between Emacs windows and external windows
managed by an external window manager / compositor like
KDE's KWin,
<ul>
<li>🔀 switching to the Emacs or desktop window at left/right/above/below or </li>
<li>🏷️ switching to another application like firefox by name, as well as</li>
<li>⧉ reposition and resize desktop windows with keys from within emacs
   instead of with the mouse.</li>
</ul>
Currently works for KDE's KWin under Wayland and X11.
It should not be difficult to extend it to other window managers or
compositors.
</p>
<hr>

## <a id="setup">1. How to setup and basic use 🏁</a>
In your `init.el` (see [example](etc/example-emacs-init/init.el)):

1. Ensure emacs server is running
   ```emacs-lisp
   (require 'server)
   (unless (server-running-p)
	   (server-start))
	```
	Also make sure:
    - your are running KDE/KWin under Wayland or X11. 
    - `emacsclient` is installed
	   <br/>--- otherwise install via your Linux package manager,
	- `kdotool` is installed 
	   <br/>--- can be installed via `cargo install kdotool`, 
	   if cargo and rust is installed; rust and cargo you can install via your Linux
 	   package manager.
2. For ⧉ **arranging desktop windows** from within emacs, add to your `init.el`:
   ```emacs-lisp
	(use-package dwin
		:straight (dwin :type git :repo "https://github.com/lsth/dwin.git")
		:config
		(dwin-setup))
	```
	Then with `M-x dwin-grab` you can grab any desktop window and resize and reposition it. 

3. For 🔀 **directional navigation**, add to `use-package`:
   ```emacs-lisp
		:config
		(global-set-key (kbd "M-<left>") #'dwin-windmove-left)
		(global-set-key (kbd "M-<right>") #'dwin-windmove-right)
		(global-set-key (kbd "M-<up>") #'dwin-windmove-up)
		(global-set-key (kbd "M-<down>") #'dwin-windmove-down))
	```
	In ⚙️ KDE/System Settings/Shortcuts you have to add shortcut keys manually
	- `Alt-left` to `etc/bin/dwin-left`, 
	- `Alt-right` to `etc/bin/dwin-right`, 
	- `Alt-up` to `etc/bin/dwin-up`, and 
	- `Alt-down` to `etc/bin/dwin-down`.
    	
    (alternatively, you can 
    - copy the contents of [etc/_config/kglobalshortcutsrc](etc/_config/kglobalshortcutsrc) at the end
      of your `~/.config/kglobalshortcutsrc` and
    - the files [etc/_local-share-applications/*.desktop](etc/_local-share-applications/) into your
      `~/.local/share/applications` and then
    - ask kwin to reload its config: `qdbus6 org.kde.KWin /KWin reconfigure`.)

    Also ensure that [_emacs-key](etc/bin/_emacs-key) is on your PATH, e.g.,
	by copying it to `~/bin` or `~/.local/bin` (if in your PATH).

	Then with `M-<left>/<right>/<up>/<down>` you can move between Emacs
	windows and between	desktop windows seamlessly.

4. For basic 🏷️ **named navigation**, add to `use-package`:
   ```emacs-lisp
       :config
	   (global-set-key (kbd "C-<f11>") #'dwin-switch-to-emacs-or)
   ```
   In ⚙️ KDE/System Settings/Shortcuts you have to add the shortcut key 
   - `Ctrl-f11` to `etc/bin/dwin-emacs`, 
   
   Then with `<f11>` you can switch from emacs to firefox and 
   with `C-<f11>`  you can switch back from firefox to emacs.


## <a id="usage">2. Further Usage 🚀</a>
1. ⧉ **Arranging desktop windows** can do the following:

| Key         | Action                                        |
|-------------|-----------------------------------------------|
| `a`         | activate window                               |
| `c`         | close window                                  |
| `M`         | minimize window (there is no maximize window) |
| `r`         | raise window                                  |
| `left`      | move to window left                           |
| `right`     | move to window right                          |
| `up`        | move to window up                             |
| `down`      | move to window down                           |
| `+`         | resize window horizontally (increase)         |
| `-`         | resize window horizontally (decrease)         |
| `M-+`       | resize window vertically (increase)           |
| `M--`       | resize window vertically (decrease)           |
| `W`         | pick another window to arrange                |
| `.`         | pick the Emacs window to arrange              |
| `D<number>` | move window to desktop                        |
| `q`         | quit                                          |

2. For **toggling** 🏷️ **named navigation** with a single key, add to `use-package`:
   ```emacs-lisp
       :config
	   (defun my/firefox (&optional prefix)
		   (interactive "P")
		   (dwin-switch-to-app "firefox" prefix))
	   (global-set-key (kbd "<f11>") #'my/firefox)

	   (defun my/zotero (&optional prefix)
		   (interactive "P")
		   (dwin-switch-to-app "zotero" prefix))
	   (global-set-key (kbd "M-<f11>") #'my/zotero)
   ```
   In ⚙️ KDE/System Settings/Shortcuts you have to add shortcut keys 
   - `f11` to `etc/bin/dwin-firefox`, and 
   - `Alt-f11` to `etc/bin/dwin-zotero`.
   
   Then you can toggle with `<f11>` between firefox and emacs. 

3. 🏷️ **Named navigation** with apps with multiple windows:
- Commands with different arguments count as different apps: `okular b.pdf` 
  will start a new window, even if `okular a.pdf` runs already.
- If there are multiple windows for the very same command, e.g., you started 
  several `okular` processes and then opened pdfs with its file dialog, the window
  to switch to will be chosen as follows:
  1. if you opened some app windows outside emacs, some from within emacs, 
    the app windows from within emacs will be preferred.
  2. if there are multiple app windows started from emacs, then the first one will be 
    activated.
  3. With `C-<number> M-x dwin-switch-to-app` (the prefix argument) you can 
    pick a specific window of the app. Once picked, it will be chosen automatically again
	when you switch to it next time.

4. Generic **global keys**:
- You can use the global key mechanism to let emacs handle any key globally and 
  execute some command. 

## <a id="impl">3. How it is implemented 🛠️</a>
dwin provides two types of navigation:
1. 🏷️ navigation by name,
   e.g., switching to firefox, zotero, back to emacs, and
2. 🔀 directional navigation,
   e.g., moving to the window to the right or below.

That navigation works globally, also outside emacs, requires that
the window manager forwards some keys globally to emacs;
for KDE one can bind a one-line script that uses emacsclient
to forward the key to emacs, using `dwin-input-key` defined in code sect. 1.
See `etc/bin/dwin-firefox` for an example. See 
<a href="#why-not-ydotool">Further Details / 1</a>
for why we cannot use tools like [ydotool](https://github.com/ReimuNotMoe/ydotool) 
for sending keys.

Emacs then can use functions provided by the window manager
to implement window navigation globally. We captured all required
methods in a window manager proxy object `dwin-proxy` whose
methods can be called via `dwin-call`. The proxy has to be created
once before use, e.g., during emacs initialization, using
`dwin-setup`. Currently only a proxy for KDE is implemented.
But it should be possible to implement further ones.
The KDE proxy uses
<ol type="i">
<li>dbus calls to org.kde.kglobalaccel (KDE's shortcuts application),
   esp. for directional navigation, and</li>
<li>kdotool for navigation by name.</li>
</ol>
See code sect. 2.

🏷️ Navigation by name is provided by `dwin-switch-to-app` that will
<ol type="i">
<li>start an app, if it has no window yet,</li>
<li>switch to its window, if it is not active, or</li>
<li>switch back to emacs, if it is already active (toggle).</li>
</ol>

For each app (firefox, zotero etc.) one needs to
<ol type="i">
<li>define one emacs command (e.g., `my/firefox`),</li>
<li>bind it to a key in emacs and</li>
<li>make sure this key is forwarded globally to emacs,
  i.e.,
  <ol type="a">
    <li>create a small script (e.g., `etc/bin/dwin-firefox`) and</li>
    <li>bind it globally to this key.</li>
	</ol>
</ol>
   
Switching back to emacs is handled by `dwin-switch-to-emacs-or`.
It needs the same handling as the other apps above.
By default, navigation by name will switch to the first window of an application,
if it has several. You can use a prefix arg to switch to a specific one,
e.g., `C-2 M-x my/firefox` or `C-2 <f11>` to switch to the second one. 
See code sect. 3.

For 🔀 directional navigation, we defined a short function
`dwin-windmove-left` for each direction. The function tries to
move inside emacs via `windmove`, and if this fails, uses the
window manager to move out of emacs.
The same method also uses the window manager to move
directional from desktop windows. See code sect. 4.

Code sect. 5 contains function `dwin-grab` to ⧉ arrange desktop windows, i.e.,
to resize them, reposition them etc. 

### Known Issues and Limitations:
1. Requesting help for a **global** key binding with `describe key` will not work.

   Emacs will get the key forwarded, but not as input for the already running
   describe-key function. In effect, if you say `M-x describe-key` and then type
   a global key like `M-<left>`, its action is executed and help for the **next**
   key the user types is shown. To view the help, you have to know how the key
   is called in emacs and say `M-: (describe-key (kbd "M-<left>"))` instead.

   One could instrument `dwin-input-key` to detect if `describe-key` is running
   by checking `(describe-key (kbd "M-<left>"))` and then run `describe-key` with
   the key instead of the command the key is bound to. However, I found no
   way to cancel the already running describe-key command. So the second issue
   will persist. 

### Further details:
1. <a id="why-not-ydotool">Why cannot we just use ydotool to send keys to emacs?</a>
   <br/>Tools like ydotool seem to be able to send key events only to the
   active/focused window and would be able only to implement the cases
   where one wants to move from within emacs, not the cases where
   one wants to move back to emacs from other applications (or
   between other applications). One could reimplement dwin in
   bash, then there is no need to input keys into emacs anymore.

   Unfortunately sending dbus events to kwin from bash was unstable
   for me: running
     ```
     qdbus6 org.kde.kglobalaccel /component/kwin \
	 org.kde.kglobalaccel.Component invokeShortcut "Switch Window Left"
	 ```
   often (not always) yielded
      ```
     Cannot find org.kde.kglobalaccel.Component in object 
	 /component/kwin at org.kde.kglobalaccel
	 ```
   This never happened when sending the same events from emacs.

## <a id="relatedwork">4. Delineation from Related Work 📚</a>

| package | X11 | Wayland | use wm | ⧉ arrange | 🏷️ named nav | 🔀 directional nav |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: |
| [exwm](https://github.com/emacs-exwm/exwm)       | ✅ | 🚫 | 🚫 | ✅ | ✅ | ✅ |
| [ewmctrl](https://github.com/flexibeast/ewmctrl) | ✅ | 🚫 | ✅ | ✅ | 🚫 | 🚫 |
| **dwin (this one)**                              | ✅<br/>(KDE) | ✅<br/>(KDE) | ✅<br/>(KDE) | ✅ | ✅ | ✅ |

1. **[exwm](https://github.com/emacs-exwm/exwm)**:
   exwm is a full X11 window manager implemented in Emacs.
   - dwin uses an existing window manager like KDE's KWin instead.
   - 👍 Integration with Emacs is way tighter: desktop windows are buffers.
   - 👎 You have to setup a lot of basic things yourself (a panel, network manager, 
      sync services etc. etc.), that usually KDE does for you.
   - 👎 exwm currently works only for X11, not for Wayland. 	

2. **[ewmctrl](https://github.com/flexibeast/ewmctrl)** lets you arrange X11 desktop windows with 
  [wmctrl](https://github.com/saravanabalagi/wmctrl).
   - limited to X11 and arranging windows.
   
3. **[kwin-minor-mode](https://github.com/emacsmirror/kwin)** allows you to communicate with 
   KDE's X11 window manager and wayland compositor [KWin](https://github.com/KDE/kwin) 
   to implement a window management API on your own with its builtin javascript engine.
   In this context, it is comparable to tools like [kdotool](https://github.com/jinliu/kdotool).
   - Provides no user facing functionality directly.

4. **[sway.el](https://github.com/thblt/sway.el)** integrates the window manager [sway](https://swaywm.org/) with Emacs.
   - <it>"The only directly usable feature as of now is the sway-shackle-display-buffer-frame function, 
  which either creates a new frame with a given buffer or focuses that buffer in the frame it’s already displayed"</it>
  (from their website).

5. **[emacs-tmux-pane](https://github.com/laishulu/emacs-tmux-pane)** integrates
   emacs windows with tmux panes.
   - So it works with a text user interface, not a graphical user interface.

6. **[ace-window](https://github.com/abo-abo/ace-window)**,
   [e2wm](https://github.com/kiwanami/emacs-window-manager),
   [edwina](https://github.com/axgfn/edwina),
   [elscreen](https://github.com/knu/elscreen),
   [elwm](https://github.com/Fuco1/elwm),
   [es-windows](https://github.com/sabof/es-windows),
   [ewm](https://github.com/laluxx/ewm/)
   and many other packages allow you to manage **Emacs windows**.
   - dwin is about managing Emacs external, **desktop windows**.
