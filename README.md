<h1 align="center">Emacs Desktop Window Manager (dwin)</h1>
<h2 align="center">Manage desktop windows from your Emacs</h2>
<p align="center">
  <a href="#setup"><b>Setup 🏁</b></a> |
  <a href="#usage"><b>Advanced Usage 🚀</b></a> |
  <a href="#impl"><b>Implementation 🛠️</b></a> |
  <a href="#relatedwork"><b>Related Work 📚</b></a> |
  <a href="#extensions"><b>Extensions 🧩</b></a>
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
Currently works for 
  any/most X11 window managers as well as for 
  KDE's KWin, both under Wayland and X11.
It should not be too difficult to extend it to other window managers or
compositors.
</p>
<hr>

## <a id="setup">1. How to setup and basic use 🏁</a>
1. Make sure
    - your are running 
	  - X11 --- any window manager compatible with [xdotool](https://github.com/jordansissel/xdotool) should do, or 
	  - KDE/KWin, either under Wayland or X11. 
    - `emacsclient` is installed
	   <br/>--- otherwise install via your Linux package manager,
	- for X11 window managers, [xdotool](https://github.com/jordansissel/xdotool) is installed,
	  <br/>--- can be installed with your package manager,
	- for KDE/KWin, kdotool is installed,
	  <br/> --- see [issue 4](#limit:kdotool) below about how to do this currently. 
<!--	   <br/>--- can be installed via `cargo install kdotool`, 
	- for KDE/KWin, [kdotool](https://github.com/jinliu/kdotool) is installed 
	   <br/>--- can be installed via `cargo install kdotool`, 
	   if cargo and rust is installed; rust and cargo you can install via your Linux
 	   package manager.
	   -->
2. For ⧉ **arranging desktop windows** from within emacs, add to your `init.el`
   (with [MELPA](https://www.melpa.org/) already configured; 
   see [example init.el](etc/example-emacs-inits/01-dwin-via-package/init.el)):
   ```emacs-lisp
   (use-package dwin
	   :ensure t
	   :config
	   (dwin-setup))
   ```
	or, if you are using the package manager [straight.el](https://github.com/radian-software/straight.el)
	(see [example init.el](etc/example-emacs-inits/02-dwin-via-straight/init.el)):
   ```emacs-lisp
   (use-package dwin
      :straight (dwin :type git :host github :repo "lsth/dwin")
      :config
      (dwin-setup))
   ```
	Then with `M-x dwin-grab` you can grab any desktop window and resize and reposition it. 

	For X11 also some irrelevant windows will be offered; see [Known Issues 2](#limit:irrelevant-windows).
	There should be no such issue with KDE/KWin.

3. For 🔀 **directional navigation**, add to `use-package`:
   ```emacs-lisp
      :config
	  (dwin-keymap-desktopglobal-set "M-<left>"  #'dwin-windmove-left)
	  (dwin-keymap-desktopglobal-set "M-<right>" #'dwin-windmove-right)
	  (dwin-keymap-desktopglobal-set "M-<up>"    #'dwin-windmove-up)
	  (dwin-keymap-desktopglobal-set "M-<down>"  #'dwin-windmove-down)
	```
	Then with `M-<left>/<right>/<up>/<down>` you can move between Emacs
	windows and between desktop windows seamlessly. 
	
	For window managers / compositors other than KDE/KWin you have to disable
	dwins automatic key setting via
   ```emacs-lisp
      :config
      (setq dwin-automatically-sync-desktop-global-keys nil)
   ```
	and use your window 
	managers mechanism to define shortcuts manually once per key, see 
	[Further Details/Key Bindings](etc/further-details.md#key-bindings). 

4. For basic 🏷️ **named navigation**, add to `use-package`:
   ```emacs-lisp
       :config
       (dwin-keymap-desktopglobal-set "C-<f11>" #'dwin-switch-to-emacs-or)
   ```
   Then with 
   `M-x dwin-switch-to-app firefox` you can switch from emacs to firefox and with 
   `C-<f11>`  you can switch back from firefox to emacs.

## <a id="usage">2. Further Usage 🚀</a>
1. ⧉ **Arranging desktop windows** can do the following:

| Key         | Action                                        |
|-------------|-----------------------------------------------|
| `a`         | activate window                               |
| `r`         | raise window                                  |
| `M`         | minimize window (there is no maximize window) |
| `c`         | close window                                  |
| `left`      | move to window on the left                    |
| `right`     | move to window on the right                   |
| `up`        | move to window above                          |
| `down`      | move to window below                          |
| `D<number>` | move window to desktop                        |
| `+`         | resize window horizontally (increase)         |
| `-`         | resize window horizontally (decrease)         |
| `M-+`       | resize window vertically (increase)           |
| `M--`       | resize window vertically (decrease)           |
| `W`         | pick another window to arrange                |
| `.`         | pick the Emacs window to arrange              |
| `q`         | quit                                          |

2. For **toggling** 🏷️ **named navigation** with a single key, add to `use-package`:
   ```emacs-lisp
       :config
	   (defun my/firefox (&optional prefix)
		   (interactive (list current-prefix-arg))
		   (dwin-switch-to-app "firefox" prefix))
       (dwin-keymap-desktopglobal-set "<f11>" #'my/firefox)

	   (defun my/zotero (&optional prefix)
		   (interactive (list current-prefix-arg))
		   (dwin-switch-to-app "zotero" prefix))
       (dwin-keymap-desktopglobal-set "M-<f11>" #'my/zotero)
   ```
   Now you can toggle with `<f11>` between firefox and emacs. 

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

5. **Switching back to launchers**.
- Launchers are short-lived programs that ask another server process to do something, 
  for example to show a webpage or PDF, and then terminate. `firefox <url>` is such a 
  launcher. Once firefox is running, the second call does not start another firefox process,
  but just asks the first one to show another URL.
- One cannot switch back to launchers based on their PID, because the created windows
  are not associated with their PID, but with the PID of the server process.
- If server processes open **new windows**, then dwin will capture those windows
  and switch back to them.
- This way you can switch back to `firefox --new-window <url>` (as it opens a new
  window), but not to `firefox --new-tab <url>` (as it does not; switching back
  to the tab would require to know which window contains tabs showing which URLs
  and a function to switch to a specific tab programmatically. Few applications have 
  these features. So for switching between Emacs and another launcher application, 
  best create new windows.

## <a id="impl">3. How it is implemented 🛠️</a>
dwin provides two types of navigation:

1. 🏷️ navigation by name,
    e.g., switching to firefox, zotero, back to emacs, and
2. 🔀 directional navigation,
   e.g., moving to the window to the right or below.

That navigation works globally, also outside emacs, requires that
the window manager forwards some keys globally to emacs;
for KDE one can bind a one-line script that uses emacsclient
to forward the key to emacs, using `dwin-input-key`.
See `etc/bin/dwin-firefox` for an example. See 
[Further Details/Why Not Ydotool](etc/further-details.md#why-not-ydotool)
for why we cannot use tools like [ydotool](https://github.com/ReimuNotMoe/ydotool) 
for sending keys.

Emacs then can use functions provided by the window manager
to implement window navigation globally. We captured all required
methods in a window manager proxy object `dwin-proxy` whose
methods can be called via `dwin-call`. The proxy has to be created
once before use, e.g., during emacs initialization, using
`dwin-setup`. Currently two proxies are implemented:

1. a generic proxy for X11 window managers using [xdotool](https://github.com/jordansissel/xdotool), and
2. a proxy for KDE on X11 or Wayland.

It should be possible to implement further ones.

The generic X11 proxy uses
<ol type="i">
<li>[xdotool](https://github.com/jordansissel/xdotool) for arranging windows and 
navigation by name and</li>
<li>a few lines elisp for directional navigation on top.</li>
</ol>

The KDE proxy uses
<ol type="i">
<li>[kdotool](https://github.com/jinliu/kdotool) for arranging windows and navigation by name and</li>
<li>dbus calls to org.kde.kglobalaccel (KDE's shortcuts application)
   for directional navigation.</li>
</ol>

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

For 🔀 directional navigation, we defined a short function
`dwin-windmove-left` for each direction. The function tries to
move inside emacs via `windmove`, and if this fails, uses the
window manager to move out of emacs.
The same method also uses the window manager to move
directional from desktop windows.

⧉ Arranging desktop windows, e.g., to move them around, 
to resize them, etc., is accomplished by function `dwin-grab`.

### Known Issues and Limitations:
1. <a id="limit:help-for-global-keys">Requesting help for a **global** key binding with 
   `describe key` will not work.</a>

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

2. <a id="limit:irrelevant-windows">Arranging windows under X11-generic shows auxiliary windows.</a>

	Thus the window list you have to pick from contains some irrelevant options. 
	It also implies, that in navigation by name window 0 sometimes may not be the window 
	you want to switch to. Just try `C-0 C-11`, `C-1 C-11`, `C-2 C-11` etc. until you are the right
	window. Next time you hit just `C-11`, dwin will go there directly.

	For KDE/KWin windows found by kdotool usually represent top-level apps, so all of them
	likely are relevant.

3. <a id="limit:x11-less-tested">A word of caution about the X11-generic proxy.</a>

	The KDE/KWin Wayland proxy I am using myself daily, so it should be fairly 
	well working most of the time. The generic X11 proxy I can only test occasionally
	and it might have more bugs.

4. <a id="limit:kdotool">A fixed version of kdotool is required.</a>
- The official version 0.2.1 of [kdotool](https://github.com/jinliu/kdotool)
  has several major bugs, for example, it cannot search for windows on a given
  desktop. Unfortunately, currently its issue tracker is stale.
- These bugs have been fixed in a [fork of kdotool](https://github.com/tvidal-net/kdotool),
  but this fork cannot be installed via `cargo install` yet. Currently, you have to do this 
  manually:
  ```bash
  git clone https://github.com/tvidal-net/kdotool.git
  cd kdotool
  cargo build --release
  cp target/release/kdotool ~/.local/bin
  ```
    (if `~/.local/bin` is in your PATH.) 
	To do so, cargo and rust have to be installed, usually via your Linux
    package manager.

5. <a id="limit:older-emacs">Running dwin on somewhat older Emacs.</a>
- dwin runs on Emacs from 29.1 onward seamlessly.
- For Emacs 28.1 and 28.2 you need to add to your `use-package`
  (see [example init.el with package](etc/example-emacs-inits/03-dwin-via-package-emacs-before-29.1/init.el)
  and [example init.el with straight](etc/example-emacs-inits/04-dwin-via-straight-emacs-before-29.1/init.el)):
   ```emacs-lisp
   :init
   (when (version< emacs-version "29.1")
	   ;; for forward compatibility (see dwin-compat.el)
	   (setf dwin-compat--set-transient-map-ORIG (symbol-function 'set-transient-map))
	   (defalias 'keymap-set #'dwin-compat--keymap-set)
	   (defalias 'set-transient-map #'dwin-compat--set-transient-map))
   ```
   Please note that this has to go to the `:init` section.
- dwin does not run on Emacs 27 or older (see [dwin-compat.el](dwin-compat.el)).

6. <a id="limit:two-emacs">Directional navigation does not work for Emacs windows of a **second emacs process**.</a>
- Everything works fine also for several Emacs **frames**.
- But if a second Emacs **process** is started, only the first one started will run the Emacs server.
- And thus, with `dwin-windmove-left` etc. you can only switch between Emacs windows of the 
  first Emacs process, not the second. The global keys never reach the second Emacs.
- Likely one could fix this with not too much work, but it seems to be such a niche
  case that I do not plan to look into this.

## <a id="relatedwork">4. Delineation from Related Work 📚</a>

| package | X11 | Wayland | use wm | ⧉ arrange | 🏷️ named nav | 🔀 directional nav |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: |
| [exwm](https://github.com/emacs-exwm/exwm)       | ✅ | 🚫 | 🚫 | ✅ | ✅ | ✅ |
| [ewmctrl](https://github.com/flexibeast/ewmctrl) | ✅ | 🚫 | ✅ | ✅ | 🚫 | 🚫 |
| **dwin (this one)**                              | ✅ | ✅<br/>(KDE) | ✅<br/>(X11,KDE) | ✅ | ✅ | ✅ |

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

7. **[eaf - the emacs application framework](https://github.com/emacs-eaf/emacs-application-framework)**
  embeds Qt applications via X11 reparenting into Emacs.
  - 👍 Like EXWM, it allows to handle external applications inside Emacs, via buffers.
  - 👎 It requires work per application to embed, it cannot embed all applications (like EXWM does). 
  - 👎 It works only for X11, not for Wayland.

## <a id="extensions">5. How to Customize and to Extend 🧩</a>
1. Adding commands to `dwin-grab`:

	Just bind them to `dwin-arrange-keymap`, for example 
	(see [example](etc/snippets/01-additional-command-for-dwin-grab.el)):
	```emacs-lisp
	(defun my-say-hello ()
		"Test function that just says hello."
		(interactive)
		(message "Hello!"))

	(defun my-show-geometry ()
		"Show position and size of `dwin-current-window."
		(interactive)
		(message "geometry: %s"
			(dwin-call dwin-proxy 'getwindowgeometry dwin-current-window)))

	(keymap-set dwin-arrange-keymap "!" #'my-say-hello)
	(keymap-set dwin-arrange-keymap "g" #'my-show-geometry)
	```
	Commands can access `dwin-current-window`, the window ID of the currently
	selected window. Bind them in `use-package :config` or after dwin has been loaded.
