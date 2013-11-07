;;;; PACKAGES AND REPOSITORIES

;; off mouse interface early in startup to avoid momentary display
;; probe first to not crash on emacs-nox
;;(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; setup repositories
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; ensure all packages we needa are installed.
(setq package-list
      '(clojure-mode markdown-mode paredit batch-mode
                     nrepl multiple-cursors
                     projectile
                     ))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))


;; tramp lets us open /sudo::/etc/files
(require 'tramp)

;;;; =============================================================
;;;; DAEMONIZE
;;;; =============================================================

;; so we can use emacsclient from other terminals
;; but dont start server if it already exists
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; its annoying always having to say "yes" to close client-opened files
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


;;;; =============================================================
;;;; NON-DEFAULT FILE MAPPINGS
;;;; =============================================================

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.bat$" . batch-mode))
(add-to-list 'auto-mode-alist '("\.md$" . markdown-mode))


;;;; =============================================================
;;;; GLOBAL DEFAULT OVERRIDES
;;;; =============================================================

;; do not require files to end in \n
(setq require-final-newline nil)
;; dont let other sysadmins override our config ;)
(setq inhibit-default-init t)

;; enable windows-y selection-behaviour
(delete-selection-mode 1)

;; makes scripts executable automatically
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; enable the current line to be hightlighted.
;;(global-hl-line-mode +1) ;or not - kills syntax high-lighting in SSH.

;; provides automatic loading after changing branches etc in git
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-auto-revert-mode t)
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ido mode just makes everything better.
(ido-mode)

;;;; =============================================================
;;;; FUNCTIONS
;;;; =============================================================

(defun my-join-line-with-next ()
  "join current line with next, without need for end + del"
  (interactive)
  (join-line -1))

(defun indent-whole-buffer ()
  "indent whole buffer and untabify it"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun indent-file-when-save ()
  "indent file when save."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (buffer-file-name)
                  (indent-whole-buffer))
              (save-buffer))))

;; automatically indents yanked (inserted/pasted) content
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode  clojure-mode scheme-mode
                                                     haskell-mode
                                                     ruby-mode
                                                     rspec-mode
                                                     python-mode
                                                     c-mode
                                                     c++-mode
                                                     objc-mode
                                                     latex-mode
                                                     plain-tex-mode
                                                     prog-mode)) ; sadly setting prog-mode alone is not enough
                (let
                    ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning)
                                 (region-end) nil))))))

;; taken from
;; emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

;; make home/ctrl-a move to beginning of indentation level.
;; from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Fix page-up/page-down behaviour when at beginning and end
;; of buffers.
;; From http://snarfed.org/emacs_page_up_page_down
(defun move-page-down-or-to-bottom ()
  (interactive)
  (condition-case nil (scroll-up)
    (end-of-buffer (goto-char (point-max)))))

(defun move-page-up-or-to-top ()
  (interactive)
  (condition-case nil (scroll-down)
    (beginning-of-buffer (goto-char (point-min)))))

(global-set-key [next] 'move-page-down-or-to-bottom)
(global-set-key [prior] 'move-page-up-or-to-top)

;; hide ^M from files with a mixture of dos and unix mode line-endings.
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; utility functions for key-definitions
(defun fkt (func target keys)
  "Sets up multiple keybindings for one function."
  (dolist (key keys)
    (funcall func (kbd key) target)))

(defun lsk (target &rest keys)
  "Sets up a keymap local keybinding for target with all keys provided."
  (fkt 'local-set-key target keys))

(defun gsk (target &rest keys)
  "Sets up a global keybinding for target with all keys provided."
  (fkt 'global-set-key target keys))


;;;; =============================================================
;;;; GLOBAL KEYBOARD DEFINITIONS
;;;; =============================================================

;; dont freeze emacs on ctrl-z
(global-unset-key (kbd "C-z"))

;; we know these ones from everywhere else
(gsk 'undo         "C-z")
(gsk 'other-window "<C-tab>")

;; make meta-j join the following line with the current one
(gsk 'my-join-line-with-next "M-j")

;; align-regexp lets you align stuff based on match
;; ab (bc)
;; bcdhskjfdhskj (dc)
;;
;; to align by second column, M-x align-regex (
(gsk 'align-regexp "M-&")

;; search for all matches of a given regex, list results
(gsk 'occur "C-c C-o")

;; rgrep is grep for emacs
(gsk 'rgrep "C-c C-g")

;; enable multiple cursors. enter or C-g to edit
(gsk 'mc/edit-lines                   "C-S-m C-S-m")
(gsk 'mc/mark-more-like-this-extended "C-S-m C-S-g")

;; default bindings (in isearch mode)
;; M-s h r   => highlight isearch results
;; M-s o      => open occur buffer with results

;;;; =============================================================
;;;; MODE CUSTOMIZATIONS
;;;; =============================================================

;; elisp
(defun my-emacs-lisp-mode-hook ()
  (indent-file-when-save)
  (paredit-mode)

  ;; enable imenu sections by ;;;;
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; paredit
(defun my-paredit-mode-hook ()
  ;; editing. keybindings which makes sense AND whic works in SSH
  (lsk 'paredit-forward-slurp-sexp  "<C-left>"   "M-[ c")
  (lsk 'paredit-forward-barf-sexp   "<C-right>"   "M-[ d")
  (lsk 'paredit-backward-slurp-sexp "<C-M-left>"  "ESC M-[ c")
  (lsk 'paredit-backward-barf-sexp  "<C-M-right>" "ESC M-[ d")

  ;; navigation
  (lsk 'paredit-backward            "<C-M-up>"    "ESC M-[ A")
  (lsk 'paredit-forward             "<C-M-down>"  "ESC M-[ B")

  (show-paren-mode 1) ; turn on paren match highlighting
  ;;(setq show-paren-style 'expression) ; highlight entire bracket expression
  )
(add-hook 'paredit-mode-hook 'my-paredit-mode-hook)

;; clojure
(defun my-clojure-mode-hook ()
  ;; key-bindings
  (lsk 'nrepl-jack-in "<f5>") ; because we know this from VS!

  ;; settings
  (indent-file-when-save)
  (paredit-mode)
  ;; for clojure comments - override annoying elisp mode single-; comment-indentation.
  (setq comment-column 0))
(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)

;; org-mode
(defun my-org-mode-hook ()
  ;; keybindings
  (lsk 'org-store-link "C-c l")
  (lsk 'org-agenda     "C-c a")
  (lsk 'org-iswitchb   "C-c b")
  ;; override C-c ¨ as that doesnt work on norwegian keyboards
  (lsk 'org-table-sort-lines "C-c s"))
(add-hook 'org-mode-hook 'my-org-mode-hook)

(defun my-prog-mode-hook ()
  ;; keybindings

  ;; not C-k C-c & C-k C-t because C-k is kill-line in emacs
  (lsk 'comment-or-uncomment-region      "C-c C-c")
  (lsk 'uncomment-region    "C-c C-u")

  ;; formatting matters in programming files
  (lsk 'indent-whole-buffer "C-i")

  ;; code - navigate to definition
  (lsk 'imenu "<f12>")

  ;; code-completion
  ;; improve it with this setup here:
  ;; http://ianeslick.com/2013/05/17/clojure-debugging-13-emacs-nrepl-and-ritz/
  (lsk 'dabbrev-expand "C-.")

  ;; settings
  ;; ensure we have line-numbers in all code-files
  ;;(global-linum-mode 1)
  (linum-mode 1)

  ;; highlight current function?
  (which-function-mode 1)

  ;; enable imenu
  (imenu-add-menubar-index)

  ;; projectile mode: on!
  ;; C-c p f - search for any file in your lein/git/etc project
  ;; more docs and bindings here: https://github.com/bbatsov/projectile
  (projectile-mode))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;;;; =============================================================
;;;; WINDOWS ONLY CUSTOMIZATIONS
;;;; =============================================================

(defun my-windows-mode-hook ()
  (add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
  (require 'powerline)

  ;; get some themes man! (black on white is tiresome)
  (add-to-list 'load-path "~/.emacs.d/vendor/color-theme-6.6.0")
  (require 'color-theme)
  (eval-after-load "color-theme"
    '(progn
       (color-theme-initialize)
       (color-theme-arjen)))

  ;; enable the current line to be hightlighted.
  (global-hl-line-mode +1))

;;;; =============================================================
;;;; UNIX ONLY CUSTOMIZATIONS
;;;; =============================================================

(defun my-unix-mode-hook ()
  ;; allow us to (automatically) open files as root when needed via tramp and /sudo::
  (require 'tramp)
  (defadvice find-file (after find-file-sudo activate)
    "Find file as root if necessary."
    (unless (and buffer-file-name
                 (file-writable-p buffer-file-name))
      (find-alternate-file (concat "/sudo::" buffer-file-name)))))

(if (eq system-type 'windows-nt)
    (my-windows-mode-hook)
  (my-unix-mode-hook))

;;;; =============================================================
;;;; FREEBSD ONLY CUSTOMIZATIONS
;;;; =============================================================

(defun my-freebsd-mode-hook ()
  ;; fix backsapce acting like delete, by making delete act like backspace.
  (normal-erase-is-backspace-mode 0)
  )
(if (eq system-type 'berkeley-unix)
    (my-freebsd-mode-hook))
