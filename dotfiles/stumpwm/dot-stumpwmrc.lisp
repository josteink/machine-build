
;; Hey, Emacs! This is a -*- lisp -*- file!
(in-package :stumpwm)

;; because it never hurts to be able to load external modules
(load "~/quicklisp/setup.lisp")
(ql:quickload "sb-posix")

(add-to-load-path "~/build/stumpwm-contrib")

;; load tray-dependencies!
;;(ql:quickload :xembed) ;; required for tray
;;(load-module "stumptray")

;; utility functions

(defun generate (from to &optional (by 1))
  #'(lambda (f)
      (when (< from to)
        (prog1 (or (funcall f from) t)
          (incf from by)))))

(defmacro with-generator ((var from to &optional (by 1)) &body body)
  (let ((generator (gensym)))
    `(loop with ,generator = (generate ,from ,to ,by)
	   while
	   (funcall ,generator
		    #'(lambda (,var) ,@body)))))

(defun append-int (text i)
  (concatenate 'string text (write-to-string i)))

(defun setup-gnome-keyring ()
  ;; GNOME Keyring
  (let* ((output (run-shell-command "/usr/bin/gnome-keyring-daemon --start" t))
	 (lines (loop :for i = 0 :then (1+ j)
		   :as j = (position #\linefeed output :start i)
		   :collect (subseq output i j)
		   :while j)))
    (dolist (line lines)
      (when (> (length line) 0)
	(let ((env-var (loop :for i = 0 :then (1+ j)
			  :as j = (position #\= line :start i)
			  :collect (subseq line i j)
			  :while j)))

	  (sb-posix:setenv (car env-var) (cadr env-var) 1))))))

(defun setup-groups ()
  ;; setup groups properly
  (with-generator (i 1 10)
    (run-commands
     (append-int "gnew Workspace" i)))
  (run-commands
   "gselect Workspace1"
   ;;"gkill Default"
   ))

;; basic stuff
(defun x-setup-once ()
  (defvar *x-setup-initialized* nil)
  (if *x-setup-initialized*
      (message "Already initialized, so skipping X setup.")
      (progn
        ;; (run-shell-command "stalonetray")
        ;;(run-shell-command "nm-applet --sm-disable")
        (run-shell-command "/usr/libexec/gnome-settings-daemon &")
        (run-shell-command "gnome-screensaver &")
        (run-shell-command "dropbox start")
        (setq *x-setup-initialized* t)

	;; activate mode-line
	(run-commands "mode-line")

	;; activate tray
	;;(stumptray)

        ;; startup stuff - safe to do this multiple times
        ;;(run-shell-command (concatenate 'string "xsetbg -border black -center " home "/Pictures/stumpwm_wallpaper.jpg"))
        (run-shell-command "emacs")
	(setup-gnome-keyring)
	(setup-groups)

	;; explicitly require swank. just in case.
        (eval '(load "/home/jostein/build/machine-build/dotfiles/stumpwm/swank.lisp"))
        (echo-string (current-screen)
                     "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))))


;; custom stumpwm-commands

(defcommand screen-saver () ()
            "Turns off the screen until a key is pressed"
            (run-shell-command
             "gnome-screensaver-command -l"))

(defmacro deflauncher (name map key)
  `(progn
     (defcommand ,(intern name) () ()
       ,(concatenate 'string "Start " name " unless it is already running, in which case focus it.")
       (run-or-raise ,name '(:class ,name)))
     (define-key ,map (kbd ,key) ,name)))


;; X setup
(x-setup-once)
;; my hotkeys  (more info on http://stumpwm.svkt.org/cgi-bin/wiki.pl)

(set-prefix-key (kbd "s-t")) ;; whatever which ISNT C-t.

(define-key *root-map* (kbd "Delete") "quit")
(define-key *top-map* (kbd "C-M-l") "screen-saver") ;; like unity/ubuntu
;; systemd can do unpriviliged suspend. auto-locked on startup.
(define-key *top-map* (kbd "C-M-p") "exec systemctl suspend")
(define-key *top-map* (kbd "s-RET") "exec gnome-terminal --hide-menubar")
(define-key *top-map* (kbd "M-Tab") "pull-hidden-next")
(define-key *top-map* (kbd "s-Tab") "fnext")
(define-key *root-map* (kbd "s-r") "loadrc")

(deflauncher "emacs" *top-map* "s-e")
(deflauncher "firefox" *top-map* "s-d")
(deflauncher "thunar" *top-map* "s-n")
(deflauncher "spotify" *top-map* "s-s")

;; audio stuff
(deflauncher "pavucontrol" *root-map* "s-v")
(deflauncher "paprefs" *root-map* "s-d")


;;(define-key *top-map* (kbd "XF86AudioRaiseVolume") "raise-volume")
;;(define-key *top-map* (kbd "XF86AudioLowerVolume") "lower-volume")
;;(define-key *top-map* (kbd "XF86AudioMute") "toggle-mute-volume")

;; i3-like workspace navigation

(defun i3-combinator (key-prefix command-prefix)
  (with-generator (i 1 10)
    (let ((key (append-int key-prefix i))
	  (cmd (append-int command-prefix i)))
      (define-key *top-map* (kbd key) cmd))))

;; s-F1 -> gselect 1
(i3-combinator "s-F" "gselect Workspace")

;; S-s-F1 -> gmove 1
(i3-combinator "S-s-F" "gmove-and-follow Workspace")
;; TODO: make gmove + gselect

;; s-1-0 -> select 1-0
(i3-combinator "s-" "select-window-by-number ")
;; manully patch 0 which isnt 10.
(define-key *top-map* (kbd "s-0") "select-window-by-number 0")

;; C-s-1-0 -> pull 1-0
(i3-combinator "C-s-" "pull ")
(define-key *top-map* (kbd "C-s-0") "pull 0")

(define-key *top-map* (kbd "s-Right") "move-focus right")
(define-key *top-map* (kbd "s-Left")  "move-focus left")
(define-key *top-map* (kbd "s-Up")    "move-focus up")
(define-key *top-map* (kbd "s-Down")  "move-focus down")

(define-key *top-map* (kbd "S-s-Right") "gnext")
(define-key *top-map* (kbd "S-s-Left") "gprev")

(define-key *top-map* (kbd "C-s-S-Right") "gnext-with-window")
(define-key *top-map* (kbd "C-s-S-Left") "gprev-with-window")

(define-key *top-map* (kbd "s-r") "iresize")
(define-key *top-map* (kbd "s-S") "vsplit")
(define-key *top-map* (kbd "s-s") "hsplit")
(define-key *top-map* (kbd "C-s-s") "remove")
(define-key *top-map* (kbd "s-q") "only")

;; swap CAPS and CTRL
;; (run-shell-command "setxkbmap -option ctrl:swapcaps")

(setq *window-format* "%n%s%80t [%20c]")

;; TODO: Modeline

;; (toggle-mode-line (current-screen)
;; 		  (current-head))

;; (setf *screen-mode-line-format*
;;       (list "%w | "
;;             '(:eval (run-shell-command "date" t))))

;; (setf *mode-line-position* :bottom)

;; clean up built-in bindings we no longer need

(dolist (key '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
	       "C-0" "C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"
	       "F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10"))
  (undefine-key *root-map* (kbd key)))

;; setup volume control key bindings
(defcommand volume-up () ()
	    "Increases master volume by 5 percent."
	    (let ((result (run-shell-command "amixer sset Master 5%+" t)))
	      (cl-ppcre:register-groups-bind (pct) ("\\[(\\d+)%\\]" result)
					     (message "~a%" pct))))
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")

(defcommand volume-down () ()
	    "Increases master volume by 5 percent."
	    (let ((result (run-shell-command "amixer sset Master 5%-" t)))
	      (cl-ppcre:register-groups-bind (pct) ("\\[(\\d+)%\\]" result)
					     (message "~a%" pct))))
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")

(defcommand volume-mute-toggle () ()
	    "Mutes/Unmutes the master volume."
	    (let ((result (run-shell-command "amixer sset Master toggle" t)))
	      (cl-ppcre:register-groups-bind (state) ("\\[(on|off)\\]" result)
					     (if (equalp state "off")
						 (message "muted.")
						 (message "unmuted.")))))
(define-key *top-map* (kbd "XF86AudioMute") "volume-mute-toggle")

