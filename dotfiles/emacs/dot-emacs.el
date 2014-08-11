

;;;; PACKAGES AND REPOSITORIES


;; off mouse interface early in startup to avoid momentary display
;; probe first to not crash on emacs-nox
;;(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; setup repositories
(add-to-list 'load-path "~/.emacs.d/local/") ;; emacs23 + package.el on debian
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; ensure all packages we need are installed.
(setq package-list
      '(clojure-mode
        markdown-mode
        paredit
        batch-mode
        nrepl
        multiple-cursors
        projectile
        expand-region
        ace-jump-mode
        undo-tree
        yasnippet
        ido-yes-or-no
        haskell-mode
        powershell-mode
        web-mode
        auto-complete
        git-commit-mode
        git-rebase-mode
        magit ;; this DOES require the above two git-modse
        ))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; tramp lets us open /sudo::/etc/files
(require 'tramp)

;; powershell-mode needs to be explicitly loaded
(require 'powershell-mode)

;;;; DAEMONIZE


;; so we can use emacsclient from other terminals
;; but dont start server if it already exists
(require 'server)

(defun server-load-hook ()
  (if (and (fboundp 'server-running-p)
	   (not (server-running-p)))
      (server-start))
  ;; its annoying always having to say "yes" to close client-opened files
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

(eval-after-load "server"
  '(server-load-hook))


;;;; NON-DEFAULT FILE MAPPINGS


(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.bat$" . batch-mode))
(add-to-list 'auto-mode-alist '("\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\.config$" . xml-mode))
(add-to-list 'auto-mode-alist '("\.ps$" . powershell-mode))
(add-to-list 'auto-mode-alist '("\.ps1$" . powershell-mode))

(add-to-list 'auto-mode-alist '("\.css$" . web-mode))
(add-to-list 'auto-mode-alist '("\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\.php$" . web-mode))


;;;; GLOBAL DEFAULT OVERRIDES

;; do not require files to end in \n
(setq require-final-newline nil)
;; dont let other sysadmins override our config ;)
(setq inhibit-default-init t)

;; enable windows-y selection-behaviour
(delete-selection-mode 1)

;; pending-delete-mode means that when a region is selected and you
;; type, the contents of that region will be overwritten.
(pending-delete-mode 1)

(defun make-scripts-executable ()
  "Makes scripts selectively executable"
  (if (not (derived-mode-p 'python-mode))
      (executable-make-buffer-file-executable-if-script-p)))

;; makes scripts executable automatically
(add-hook 'after-save-hook
          'make-scripts-executable)

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
(ido-yes-or-no-mode)

;; always follow symlinks to files under source-control. dont ask.
(setq vc-follow-symlinks t)

;; load all YASnippets, but dont enable global mode.
(require 'yasnippet)
(yas-reload-all)

;; UTF-8 as default encoding. this is required for unix->windows safe dropbox transfers.
(set-language-environment "UTF-8")


;;;; FUNCTIONS


(defun my-join-line-with-next ()
  "join current line with next, without need for end + del"
  (interactive)
  (join-line -1))

(defun is-not-whitespace-language-p ()
  (not (derived-mode-p 'python-mode)))

(defun is-lisp-p ()
  (derived-mode-p 'emacs-lisp-mode 'clojure-mode 'scheme-mode))

(defun indent-whole-buffer ()
  "indent whole buffer and untabify it"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; utility-function for other functions
(defun region-str-or-symbol ()
  "Return the contents of region or current symbol."
  (if (region-active-p)
      (buffer-substring-no-properties
       (region-beginning)
       (region-end))
    (thing-at-point 'symbol)))

;; occur which has currently selected text as default value.
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (region-str-or-symbol) regexp-history)
  (call-interactively 'occur))


;; automatically indents yanked (inserted/pasted) content
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode
                        '(emacs-lisp-mode
                          lisp-mode
                          clojure-mode
                          scheme-mode
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

;; Windows-only window-control functions.

(defun w32-maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))

(defun w32-restore-frame ()
  "Restore a minimized/maximized frame"
  (interactive)
  (w32-send-sys-command 61728))

(defun w32-minimize-frame ()
  "Minimizes the current frame"
  (interactive)
  (w32-send-sys-command 61472))

;; START active region-mode

(defvar active-region-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-minor-mode active-region-mode
  "Active Region minor mode."
  :init-value nil
  :lighter " Region"
  :keymap active-region-mode-map
  :group 'active-region
  )

(defun active-region-on ()
  (active-region-mode 1))
(defun active-region-off ()
  (active-region-mode -1))
(add-hook 'activate-mark-hook 'active-region-on)
(add-hook 'deactivate-mark-hook 'active-region-off)

;; END active-region mode

;; fix default indentation to not deactivate selected region.

(setq standard-indent 2)

(defadvice increase-left-margin (after keep-transient-mark-active ())
  "Override the deactivation of the mark."
  (setq deactivate-mark nil))
(ad-activate 'increase-left-margin)

(defadvice decrease-left-margin (after keep-transient-mark-active ())
  "Override the deactivation of the mark."
  (setq deactivate-mark nil))
(ad-activate 'decrease-left-margin)

;; handle indentation properly
(defun org-return-and-indent ()
  (interactive)

  (org-return)
  (indent-according-to-mode))

;; utility functions for key-definitions
(defun fkt (func target keys)
  "Sets up multiple keybindings for one function."
  (dolist (key keys)
    ;; dont call KBD directly. that's going to fail, at least in emacs23
    ;; ref http://stackoverflow.com/questions/7549628/whats-wrong-with-the-following-unbind-script
    (funcall func (read-kbd-macro key) target)))

(defun lsk (target &rest keys)
  "Sets up a keymap local keybinding for target with all keys provided."
  (fkt 'local-set-key target keys))

(defun gsk (target &rest keys)
  "Sets up a global keybinding for target with all keys provided."
  (fkt 'global-set-key target keys))

;; ido-imenu
;; taken from https://gist.github.com/magnars/2360578
(require 'imenu)
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
  Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;; lets us pop mark and get back to where we were.
(defadvice ido-imenu (before push-mark activate)
  (push-mark))


;;;; GLOBAL KEYBOARD DEFINITIONS



;; dont freeze emacs on ctrl-z
(global-unset-key (kbd "C-z"))

;; to be able to select text in stumpwm
(gsk 'set-mark-command "C-|")

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
(gsk 'occur-dwim "C-c C-o")

;; rgrep is grep for emacs
(gsk 'rgrep "C-c C-g")

(gsk 'magit-status "C-x v g")

;; general text-completion. enable everywhere.
;; improve it with this setup here:
;; http://ianeslick.com/2013/05/17/clojure-debugging-13-emacs-nrepl-and-ritz/
(gsk 'hippie-expand "C-.") ;;(gsk 'dabbrev-expand "C-.")

;; multiple-cursors setup. doesn't come with any bindings by default
;; puts a cursor on everyline of a selected region.
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; Other MC-keybindings
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; default bindings (in isearch mode)
;; M-s h r   => highlight isearch results
;; M-s o      => open occur buffer with results

;; expand-region
(global-set-key (kbd "C-+") 'er/expand-region)

;; ace jump - remember C-u C-c SPC etc
;;(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; undo-tree - enable globally
(global-undo-tree-mode 1)
(gsk 'undo-tree-redo "C-M-z") ;; quick access to redo.
;; use default-binding C-x U for visualize.

;; special global keybindings for active regions minor-mode

;; keyboard macros
;; default bindings, but documenting them makes them easier to remember.
;; f3         - start new macro
;; f4         - complete macro
;; f4         - use recorded macro
;; C-u <x> f4 - use recorded macro <x> times
;; C-u 0 f4   - use recorded macro until end of buffer is reached.
;; C-<0-9> f4 - use recorded macro 1-9 times or until end of buffer is reached.
;; M-x insert-kbd-macro

;; indent properly, always
(gsk 'newline-and-indent "RET")


;;;; MODE CUSTOMIZATIONS


;; active-region minor mode
(defun my-active-region-mode-hook ()
  ;; dont override whatever mode emacs thinks we're in with stuff. adress the mode-map specificly.
  (define-key active-region-mode-map (kbd "<tab>") 'increase-left-margin)
  (define-key active-region-mode-map (kbd "<S-tab>") 'decrease-left-margin)
  ;; required for X11
  (define-key active-region-mode-map (kbd "<backtab>") 'decrease-left-margin))
(add-hook 'active-region-mode-hook 'my-active-region-mode-hook)

;; elisp
(defun my-emacs-lisp-mode-hook ()
  (paredit-mode)

  ;; enable imenu sections by ;;;;
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; common lisp
(defun my-lisp-mode-hook ()
  (paredit-mode))
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)

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
  (paredit-mode)
  ;; for clojure comments - override annoying elisp mode single-; comment-indentation.
  (setq comment-column 0))
(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)

;; haskell
(defun my-haskell-mode-hook ()
  ;; we want proper indentation
  (haskell-indent-mode +1))
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

;; c/c++
(defun my-c-mode-hook ()
  ;; required for auto-completion
  (semantic-mode 1))
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

;; org-mode
(defun my-org-mode-hook ()
  ;; keybindings
  (lsk 'org-store-link "C-c l")
  (lsk 'org-agenda     "C-c a")
  (lsk 'org-iswitchb   "C-c b")
  ;; override C-c ¨ as that doesnt work on norwegian keyboards
  (lsk 'org-table-sort-lines "C-c s")

  ;; we want proper new-line indentation
  ;;(lsk 'org-return-and-indent "RET")

  ;; enable imenu
  (imenu-add-menubar-index))
(add-hook 'org-mode-hook 'my-org-mode-hook)

(defun my-prog-mode-hook ()
  ;; keybindings

  ;; not C-k C-c & C-k C-t because C-k is kill-line in emacs
  (lsk 'comment-or-uncomment-region      "C-c C-c")
  (lsk 'uncomment-region    "C-c C-u")

  ;; code - navigate to definition
  (lsk 'ido-imenu "<f12>")
  ;; navigate back again.
  ;; (could also use set-mark with prefix argument C-u C-spc.)
  (lsk 'pop-global-mark "C--")

  ;; auto-complete is a must
  (auto-complete-mode 1)

  ;; settings
  ;; ensure we have line-numbers in all code-files
  ;;(global-linum-mode 1)
  (linum-mode 1)

  ;; highlight current function?
  (which-function-mode 1)

  ;; enable imenu - only for true prog-mode major-modes
  (if (derived-mode-p 'prog-mode)
      (imenu-add-menubar-index)
    'nil)

  ;; projectile mode: on!
  ;; C-c p f - search for any file in your lein/git/etc project
  ;; more docs and bindings here: https://github.com/bbatsov/projectile
  (projectile-mode)

  ;; yasnippet
  (yas-minor-mode)

  ;; formatting matters in programming files, but python is a silly
  ;; language which cares about white-space.
  (when (is-lisp-p)
    (lsk 'indent-whole-buffer "C-i")))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(add-hook 'powershell-mode-hook 'my-prog-mode-hook)

;; xml
(defun my-xml-mode-hook()
  ;; not C-k C-c & C-k C-t because C-k is kill-line in emacs
  (lsk 'comment-or-uncomment-region      "C-c C-c")
  (lsk 'uncomment-region    "C-c C-u"))

(add-hook 'nxml-mode-hook 'my-xml-mode-hook)


;; html/web
(defun my-web-mode-hook ()
  (lsk 'web-mode-comment-or-uncomment      "C-c C-c")
  (lsk 'web-mode-uncomment    "C-c C-u"))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; css should be prog-mode but isn't
(add-hook 'css-mode-hook 'my-prog-mode-hook)

;;;; WINDOWS ONLY CUSTOMIZATIONS


(defun my-windows-mode-hook ()
  ;;(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
  ;;(require 'powerline)

  ;; get some themes man! (black on white is tiresome)
  ;;(add-to-list 'load-path "~/.emacs.d/vendor/color-theme-6.6.0")
  ;; (require 'color-theme)
  ;; (eval-after-load "color-theme"
  ;;   '(progn
  ;;      (color-theme-initialize)
  ;;      (color-theme-arjen)))

  ;; free up M-SPC to allow prefixed commands.
  (global-unset-key (kbd "M-SPC"))

  ;; minimize/maximize windows as normally
  (gsk 'w32-minimize-frame "M-SPC n" "M-SPC M-n")
  (gsk 'w32-maximize-frame "M-SPC x" "M-SPC M-x")

  ;; set alternate path for diff-tool because windows does not come
  ;; with diff.exe preshipped.
  ;; required for ediff.
  ;; see more: http://stackoverflow.com/questions/7423921/how-can-i-use-ediff-under-windows-ntemacs
  (setq ediff-diff-program "C:\\cygwin64\\bin\\diff.exe")
  (setq ediff-diff3-program "C:\\cygwin64\\bin\\diff3.exe"))

;;;; UNIX ONLY CUSTOMIZATIONS


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


;;;; FREEBSD ONLY CUSTOMIZATIONS


(defun my-freebsd-mode-hook ()
  ;; fix backsapce acting like delete, by making delete act like backspace.
  (normal-erase-is-backspace-mode 0)
  )
(if (eq system-type 'berkeley-unix)
    (my-freebsd-mode-hook))


(defun my-gui-mode-hook ()
  ;; only activate global-line mode when on X11/windows/non-terminal environment.
  ;; will deactivate syntax highlighting and more in SSH.
  (global-hl-line-mode +1)

  ;; same with column-numbers.
  (column-number-mode +1)

  ;; substitute lambdas with fancy symbols
  ;; (font-lock-add-keywords
  ;;  nil `(("(\\(lambda\\>\\)"
  ;;         (0 (progn (compose-region (match-beginning 1) (match-end 1)
  ;;                                   ,(make-char 'greek-iso8859-7 107))
  ;;                   nil)))))
  ;; (setq font-lock t)
  )

(if (display-graphic-p)
    (my-gui-mode-hook)
  'nothing)

(defun my-x-mode-hook ()
  (define-key key-translation-map [dead-grave] (lookup-key key-translation-map "\C-x8`"))
  (define-key key-translation-map [dead-acute] (lookup-key key-translation-map "\C-x8'"))
  (define-key key-translation-map [dead-circumflex] (lookup-key key-translation-map "\C-x8^"))
  (define-key key-translation-map [dead-diaeresis] (lookup-key key-translation-map "\C-x8\""))
  (define-key key-translation-map [dead-tilde] (lookup-key key-translation-map "\C-x8~"))
  (define-key isearch-mode-map [dead-grave] nil)
  (define-key isearch-mode-map [dead-acute] nil)
  (define-key isearch-mode-map [dead-circumflex] nil)
  (define-key isearch-mode-map [dead-diaeresis] nil)
  (define-key isearch-mode-map [dead-tilde] nil))

;; special workaround for dead keys needed only when running emacs in Linux & X.
(if (and (display-graphic-p)
         (eq system-type 'gnu/linux))
    (my-x-mode-hook)
  'nothing)
