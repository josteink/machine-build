
;;;; Custom

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

;;;; PACKAGES AND REPOSITORIES


;; off mouse interface early in startup to avoid momentary display
;; probe first to not crash on emacs-nox
;;(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; setup repositories
(add-to-list 'load-path "~/.emacs.d/local/") ;; emacs23 + package.el on Debian
(require 'package)

(defun make-repo-url (path)
  (let ((prefix (if (gnutls-available-p) "https" "http")))
    (concat prefix "://" path)))

(ignore-errors
  (package-initialize))

(add-to-list 'package-archives `("marmalade"    . ,(make-repo-url "marmalade-repo.org/packages/")))
(add-to-list 'package-archives `("melpa"        . ,(make-repo-url "melpa.org/packages/")))
(add-to-list 'package-archives `("melpa stable" . ,(make-repo-url "stable.melpa.org/packages/")))


(add-to-list 'package-archives '("org-mode"     . "http://orgmode.org/elpa/"))

;; ensure all packages we need are installed.
(setq my-packages
      '(;;clojure-mode
        markdown-mode
        paredit
        multiple-cursors
        projectile
        expand-region
        undo-tree
        helm helm-projectile
        bmx-mode
        ido-yes-or-no
        imenu-anywhere
        web-mode
        langtool
        company
        ;; company-c-headers
        company-web
        ;; cmake-mode
        ;; ggtags
        helpful
        ;; slime-company ;; if loading fails with recursive load, check if distro-provided slime is installed.
        magit git-timemachine
        elisp-slime-nav elisp-refs
        macrostep
        flycheck flycheck-package
        ;;csharp-mode
        ;;omnisharp
        js2-mode
        json-mode
        ssh-config-mode
        ;; elfeed
        org ;; we want a newer version than the one built in!
        marmalade-client
        nodejs-repl
        crontab-mode
        highlight-symbol
        realgud
        ;; for rust
        rust-mode cargo toml-mode
        lsp-mode lsp-flycheck ;; rust does LSP!
;;      typescript-mode
;;      tide
        ;;ts-comint
        ;; python elpy yasnippet ;; needed for elpy
        yasnippet
        yaml-mode
        ))

;; only query package sources when package is missing! copied from:
;; https://github.com/zirrostig/emacsd/blob/master/my-packages/my-packages.el

(require 'cl)
(defun my-packages-installed-p ()
  "Return nil if there are packages that are not installed."
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun my-packages-install-packages ()
  "Install missing packages."
  (unless (my-packages-installed-p)
    ;; Referesh package lists
    (package-refresh-contents)
    ;; Install missing
    (dolist (p my-packages)
      (when (not (package-installed-p p))
        (ignore-errors
          (package-install p))))))

(defun my-windows-cookie-cleanup ()
  "Fix for issue which corrupts emacs' http client.

This will among other things caused emacs to break in a million ways
and packages never to download (and thus ruining the self-bootstrapping
process if the profile is 'correupted':

https://emacs.stackexchange.com/questions/15020/eww-error-in-process-sentinel-url-cookie-generate-header-lines-wrong-type-arg/15153#15153"

  (when (eq system-type 'windows-nt)
    (delete-file
     (expand-file-name "~/.emacs.d/url/cookie"))))

;; INSTALL THE PACKAGES!!!
(my-packages-install-packages)

;; Configure GUI as early as possible. It makes loading look nicer :)

(defun font-exists-p (font)
  "Probes for the existence of a font."
  (member font (font-family-list)))

(defun try-set-default-font (font size)
  "Attempt to set default-font, if present on system"

  (when (font-exists-p font)
    (let ((FONT (concat font "-" (number-to-string size))))
      (add-to-list 'default-frame-alist `(font . ,FONT ))
      (set-face-attribute 'default t :font FONT)
      (set-frame-font FONT))))

(defun my-ert-run-tests ()
  (interactive)
  (ert-run-tests-interactively t)

  ;; (kill-buffer "*ert*")
  ;; (message "All tests good!")
  )

(defun my-gui-mode-hook ()
  ;; activate theme early!

  ;; make things look funky and match stump-wm
  ;;(color-theme-initialize) ;;uncomment to load all themes
  ;; (color-theme-jsc-dark) ;; also a candidate
  ;; (require 'color-theme-gruber-darker)
  ;; (color-theme-gruber-darker)

  (ignore-errors
    (let* ((themes-dir "~/.emacs.d/themes/")
           (seti-theme (concat themes-dir "seti-theme.el")))
      (when (not (file-exists-p themes-dir))
        (make-directory themes-dir))
      (when (not (file-exists-p seti-theme))
        (url-copy-file "https://raw.githubusercontent.com/caisah/seti-theme/master/seti-theme.el" seti-theme))

      (add-to-list 'custom-theme-load-path themes-dir)
      (load-theme 'seti)
      (set-face-foreground font-lock-comment-delimiter-face "#c06000")
      (set-face-foreground font-lock-comment-face "#707070")
      (set-face-background hl-line-face "#004000")
      (set-face-foreground font-lock-variable-name-face "#55dbB5")))

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

  ;; font thingie, downloaded from http://sourcefoundry.org/hack/
  (try-set-default-font "Hack" 12)
  ;; KDE, Hidpi laptop
  ;; (try-set-default-font "Droid Sans Mono" 11)
  )

(when (display-graphic-p)
  (my-gui-mode-hook))

;; support frames created by emacsclients when Emacs is launched as a
;; persistent daemon.
(defun my-after-create-frame-function (frame)
  (my-gui-mode-hook))
(add-hook 'after-make-frame-functions #'my-after-create-frame-function)


(defun my-light-guide-mode ()
  "A bright UI-mode which better fits things like wsd-mode."

  (interactive)
  (color-theme-initialize)
  (color-theme-emacs-nw)
  (set-face-background hl-line-face "#c0c0c0"))

;; powershell-mode needs to be explicitly loaded, but only on supported platforms!
(ignore-errors
  (require 'powershell-mode))

;;;; DAEMONIZE


(defun server-load-hook ()
  ;; attempt start server. if already running, fail silently.
  (ignore-errors
    (ignore-errors
      (require 'xref))
    (if (and (fboundp 'server-running-p)
             (not (eq (server-running-p) t)))
        (server-start))
    ;; its annoying always having to say "yes" to close client-opened files
    (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)))

(eval-after-load "server"
  '(server-load-hook))

;; so we can use emacsclient from other terminals
;; but dont start server if it already exists
(require 'server)


;;;; NON-DEFAULT FILE MAPPINGS

(defun add-extensions-to-mode (mode &rest extensions)
  "Register the provided `extensions' to handle the provided `mode'."
  (dolist (item extensions)
    (let ((rx (concat "\\." item "$")))
      (add-to-list 'auto-mode-alist (cons rx mode)))))

(add-extensions-to-mode 'clojure-mode "cljs") ;; clojure-script too!
(add-extensions-to-mode 'markdown-mode "md")
(add-extensions-to-mode 'message-mode "somail")

(add-to-list 'auto-mode-alist '("www\\..*\\.txt$" . markdown-mode)) ;; it's all text, firefox extension!
(add-to-list 'auto-mode-alist '("github\\.com.*\\.txt$" . markdown-mode)) ;; it's all text, firefox extension!
(add-to-list 'auto-mode-alist '("news\\.ycombinator\\.com.*\\.txt$" . markdown-mode)) ;; hacker news is all markdown.
(add-to-list 'auto-mode-alist '("www.websequencediagrams.com.*\\.txt$" . wsd-mode)) ;; obviously
(add-to-list 'auto-mode-alist '("crontab" . crontab-mode))


(add-extensions-to-mode 'nxml-mode "config" "merge" "*proj" "xaml") ;; .NET, SuperOffice config-merge.
(add-extensions-to-mode 'powershell-mode "ps" "ps1")

;; we DONT want web-mode for CSS, because it breaks company-mode completion.
(add-extensions-to-mode 'web-mode "html" "php" "ascx" "aspx")
(add-extensions-to-mode 'js2-mode "js")

;; hook js2-mode in for shell scripts running via node.js
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;;; GLOBAL DEFAULT OVERRIDES

;; do not require files to end in \n
(setq require-final-newline nil)
;; dont let other sysadmins override our config ;)
(setq inhibit-default-init t)

;; supress splash-screen.
(setq inhibit-startup-screen t)

;; "big files" are a thing of the past
(setq large-file-warning-threshold nil)

;; supress magit-nagging
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil)

(setq org-support-shift-select t)

;; always update files when changing git-branches, etc.
(global-auto-revert-mode t)

;; enable windows-y selection-behaviour
;; (delete-selection-mode 1)

;; pending-delete-mode means that when a region is selected and you
;; type, the contents of that region will be overwritten.
(pending-delete-mode 1)

;; set all search to case insensitive
(setq case-fold-search t)

;; avoid ever loading old code from outdated ELC-files
(setq load-prefer-newer t)

;; org-mode fontified properly for babel
(setq org-src-fontify-natively t)

;; enable narrowing and widening of buffers via C-x n n and C-x n w
(put 'narrow-to-region 'disabled nil)

(defcustom my-enable-omnisharp t
  "Whether to enable omnisharp in chsarp-mode or not"
  :type 'boolean
  :group 'my)

(defun my-enable-flyspell-mode (prog-mode)
  "Enable flyspell, but only on platforms where it works well."

  ;; it basically boils down to flyspell is slow on windows,
  ;; so don't auto-enable it by default for all and any modes.
  (when (not (eq system-type 'windows-nt))
    (if prog-mode
        (flyspell-prog-mode)
      (flyspell-mode))))

;; ensure all occur-buffers have unique names (to enable multple ones)
;; (add-hook 'occur-hook 'occur-rename-buffer)

(defun make-scripts-executable ()
  "Makes scripts selectively executable"
  (if (not (derived-mode-p 'python-mode))
      (executable-make-buffer-file-executable-if-script-p)))

;; makes scripts executable automatically
(add-hook 'after-save-hook
          'make-scripts-executable)

(defun dired-focus-current-file ()
  "Go to dired with focus on the current file."
  (interactive)
  (when buffer-file-name
    (let ((file buffer-file-name))
      (dired (file-name-directory file))
      (search-forward (file-name-nondirectory file)))))

(defun describe-face-no-hl ()
  (interactive)

  (global-hl-line-mode -1)

  (let ((face (face-at-point)))
    (global-hl-line-mode t)
    (highlight-symbol-mode -1)
    (describe-face face)))

;; make compilation-mode not die when tools provide ansi-output
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; helm everywhere
(ignore-errors
  (require 'helm-config)
  (helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r l") 'helm-bookmarks)
  (setq projectile-completion-system 'helm)
  ;; (global-set-key (kbd "C-c p h") 'helm-projectile)
  )

;; still useful, even with helm.
(ignore-errors
  (ido-yes-or-no-mode))


;; always follow symlinks to files under source-control. dont ask.
(setq vc-follow-symlinks t)

;; UTF-8 as default encoding. this is required for unix->windows safe dropbox transfers.
(set-language-environment "UTF-8")

;; generally speaking we always want spaces, not tabs.
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; try to set indentation consistently.
(setq c-basic-indent 4)
(setq tab-width 4)
(setq web-mode-code-indent-offset 4)
(setq js-indent-level 4)

;; use node for JS-execution
(setq inferior-js-program-command "node --interactive")

(setq js2-use-font-lock-faces t)

;; Handling packages which relies on binaries (like JARs)

(setq emacs-binaries-folder "~/.emacs.d/binaries/")
(defun download-binary-if-missing (url)
  "Downloads a binary file to the emacs binary directory if not already there."

  (when (not (file-exists-p emacs-binaries-folder))
    (make-directory emacs-binaries-folder))

  (let* ((filename (file-name-nondirectory url))
         (local-filename (concat emacs-binaries-folder filename)))
    (when (not (file-exists-p local-filename))
      (url-copy-file url local-filename))
    local-filename))

;; plantuml-support needs to be manually wired up, both the JAR and org-mode.
(setq org-plantuml-jar-path (download-binary-if-missing "http://freefr.dl.sourceforge.net/project/plantuml/plantuml.jar"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))


;; company mode settings
(setq company-tooltip-align-annotations t)

;;;; FUNCTIONS


(defun my-join-line-with-next ()
  "join current line with next, without need for end + del"
  (interactive)
  (join-line -1))

(defun is-not-whitespace-language-p ()
  (not (derived-mode-p 'python-mode)))

(defun is-lisp-p ()
  (derived-mode-p 'emacs-lisp-mode 'clojure-mode 'scheme-mode 'lisp-mode))

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

(defun previous-error-dwim ()
  "Customized version of `previous-error' which never causes failures and just does the right thing."
  (interactive)
  (condition-case nil
      (previous-error)
    (error (first-error))))

(defun next-error-dwim ()
  "Customized version of `next-error' which never causes failures."
  (interactive)
  (ignore-errors
    (next-error)))

(defun my-move-to-start-of-word ()
  "Function to move us to the beginning of the currently selected word."
  (forward-word)
  (backward-word))

(defun my-switch-to-window-by-buffer-name (buffer)
  "Select `BUFFER' by name, and move to the window it already has."
  (interactive
   (let ((buffer-names
          (mapcar (lambda (window) (buffer-name (window-buffer window)))
                  (window-list))))
     (list (completing-read "Buffer: " buffer-names))))
  (select-window (get-buffer-window buffer)))

;; upcase and dwoncase DWIM
(defun upcase-word-dwim (arg)
  (interactive "p")

  ;; must be at start of word to upcase entire word.
  (my-move-to-start-of-word)
  (upcase-word arg))

(defun downcase-word-dwim (arg)
  (interactive "p")

  ;; must be at start of word to upcase entire word.
  (my-move-to-start-of-word)
  (downcase-word arg))

(defun nxml-where (arg)
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive "p")
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (let* ((result (concat "/" (mapconcat 'identity path "/"))))
              (if (= 1 arg)
                  (message result)
                (progn
                  (kill-new result)
                  (message "Path %s added to kill-ring." result)))))))))

(defun nxml-pretty-print-buffer ()
  "Pretty format XML markup in the buffer.

The function inserts line-breaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nxml's indentation rules."
  (interactive)

  (when (not (derived-mode-p 'nxml-mode))
    (nxml-mode))

  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-whole-buffer))
  (message "Ah, much better!"))


;; automatically handle DOS EOL and silence it
(defun my-find-file-hook ()
  (interactive)

  (ignore-errors
    (when (file-exists-p (buffer-file-name))
      (setq file-line (thing-at-point 'line))
      (setq file-line-match (string-match-p (regexp-quote "^M") file-line))

      ;; will contain ^M if dos-eol is active
      (if (not (eq :nil file-line-match))
          (remove-dos-eol)))))
(add-hook 'find-file-hook 'my-find-file-hook)


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
                          nxml-mode
                          prog-mode)) ; sadly setting prog-mode alone is not enough
                (let
                    ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning)
                                 (region-end) nil))))))

(defun get-clipboard-no-properties ()
  "Gets the current clipboard/`kill-ring' item with all metadata and properties stripped out."

  (let* ((contents (car kill-ring))
         ;; strip out fontification metadata
         (ignored  (set-text-properties 0 (length contents) 'nil contents)))
    contents))

(defun escape-string (text)
  "Escapes the provded string so that itself will fit inside a string-constant.

  TEXT: string to escape"

  ;; quoted actually has start and end quotes abc -> "abc".
  ;; remove quotes to get escaped text only.
  (let* ((quoted (format "%S" text)))
    (substring quoted 1 (- (length quoted) 1))))

(defun yank-quote ()
  "Automatically quotes and escapes the clipboard-data."
  (interactive)

  (insert (escape-string (get-clipboard-no-properties))))

(defun point-at-string-p ()
  "Returns true if point is in a string-context"

  ;; search-backward is not bullet-proof, but a decent fallback for
  ;; paredit-backward-up, because that only works in lisp-modes.
  (cl-flet ((search-up () (condition-case nil
                              (paredit-backward-up)
                            (error (search-backward "\"")))))
    (let* ((s1 (sexp-at-point))                           ;; |"you are here" - not valid!
           (s2 (save-excursion                            ;; "|you are here" - valid
                 (ignore-errors (search-up))
                 (sexp-at-point)))
           (s3 (save-excursion                            ;; "you |are here" - valid
                 (ignore-errors (search-up)
                                (search-up))
                 (sexp-at-point))))
      ;; if the symbol is a string, the point is at a string.
      (or (stringp s2)
          (stringp s3)))))

(defun yank-quote-dwim (r)
  "Yank with automatic quoting when inside a string-context."
  (interactive "p")

  (if (point-at-string-p)
      (yank-quote)
    (yank r)))

(defun kill-line-dwim (r)
  "Tries to do the 'right thing' when killing a line."
  (interactive "p"))

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

;; assist helm-completion. I -never- type auto-fill with a dash...
(setq autofill-mode #'auto-fill-mode)


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

(require 'imenu)
(defun imenu-nav-dwim ()
  "Call `imenu' for the current symbol."
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (let ((symbol (thing-at-point 'symbol)))
    (imenu symbol)))

(defun msbuild ()
  "Trigger msbuild for the current directory."
  (interactive)

  (let ((compile-command "C:\\Windows\\Microsoft.NET\\Framework64\\v4.0.30319\\msbuild.exe"))
    (recompile)))

;; copy-pasted from
;; http://stackoverflow.com/questions/20426160/how-to-pop-local-and-global-marks-with-a-common-keybinding-in-emacs
(defun pop-local-or-global-mark ()
  "Pop to local mark if it exists or to the global mark if it does not."
  (interactive)
  (if (mark t)
      (pop-to-mark-command)
    (pop-global-mark)))

;; commenting out sexps is easy: C-M-<SPC> M-; but uncommenting them is harder...
;; this function solves this. copied from:
;; http://endlessparentheses.com/a-comment-or-uncomment-sexp-command.html?source=rss
(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

;; typescript helper functions

(defun string-no-properties (value)
  (substring-no-properties value 0 (length value)))

(defun my-ts-create-function ()
  (interactive)

  (let* ((symbol        (symbol-at-point))
         (name          (symbol-name symbol))
         (function-name (string-no-properties name)))
    (prin1 (concat "Creating function " function-name "."))

    (backward-up-list)
    (beginning-of-line)
    (newline-and-indent)
    (newline-and-indent)
    (forward-line -2)
    (insert (concat "function " function-name "() {"))
    (newline-and-indent)
    (newline-and-indent)
    (insert-char ?})
    (newline-and-indent)
    (forward-line -2)
    (indent-according-to-mode)))

(defun my-ts-tsc ()
  "Compile code using tsc without changing default-command for `compile' or `recompile'."
  (interactive)
  (let* ((compile-command))
    (compile "tsc")))


;; make nodejs nicer to work with

(defun my-nodejs-send-region-to-repl (start end)
  "Sends the currently highlighted region to the nodejs repl."
  (interactive "r")

  (let* ((proc (get-process nodejs-repl-process-name)))
    (comint-send-region proc start end)
    ;; in case newline is not included at end of region.
    (comint-send-string proc "\n")))

(defun my-nodejs-eval-buffer ()
  "Sends the enture current buffer to the nodejs repl."
  (interactive)

  (my-nodejs-send-region-to-repl
   (point-min)
   (point-max)))

;; Windbg helper

(defface hi-yellow-b
  '((((min-colors 88)) (:weight bold :foreground "yellow1"))
    (t (:weight bold :foreground "yellow")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-cyan-b
  '((((min-colors 88)) (:weight bold :foreground "cyan1"))
    (t (:weight bold :foreground "cyan")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defun windbg-mode ()
  "Provide simple highlighting of common constructs in windbg log output"
  (interactive)

  ;; highlight prompts
  (hi-lock-face-buffer "[0-9]+:[0-9]+>" 'hi-yellow)

  ;; highlight typical commands
  (hi-lock-face-buffer "\s!\\w+" 'hi-green-b)
  (hi-lock-face-buffer "\s\\.\\w+" 'hi-green-b)
  ;; monitors are always ugly
  (hi-lock-face-buffer "Monitor.Enter" 'hi-red-b)
  ;; exceptions
  (hi-lock-face-buffer "[A-Za-z]+Exception" 'hi-red-b)

  ;; Some things should light up subtly in log
  (hi-lock-face-buffer "SuperOffice[A-Za-z0-9\s\\._\\+-]+" 'hi-cyan-b))

;; superoffice-specific customization for cdb-comint-mode
(defun my-cdb-custom-highlight-function ()
  ;; Some things should light up subtly in log
  (hi-lock-face-buffer "SuperOffice[A-Za-z0-9\s\\._\\+-]+" 'hi-cyan-b))
(setq cdb-custom-update-function 'my-cdb-custom-highlight-function)


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

;; mode-helpers
(defun get-active-modes ()
  "Returns a list of the currently active modes."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    active-modes))

(defun is-mode-active-p (minor-mode)
  "Returns if the specified minor-mode is active or not."
  (interactive)
  (member minor-mode (get-active-modes)))

(defmacro defhook (hook-name &rest body)
  (let* ((my-hook-name (intern (concat "my-" (symbol-name hook-name)))))
    `(progn
       (defun ,my-hook-name ()
         ,@body)
       (add-hook ',hook-name ',my-hook-name))))


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

(setq active-region-restore-autofill nil)

(defun active-region-on ()
  (active-region-mode 1)

  ;; restore auto-fill-mode when active.
  (setq active-region-restore-autofill (not (null auto-fill-function)))
  (auto-fill-mode -1))

(defun active-region-off ()
  (active-region-mode -1)
  (when active-region-restore-autofill
    (auto-fill-mode 1)))

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

(defun looking-back-at-p (args)
  "Checks if the current point is preceeded by any of the provided arguments."
  (let* ((result nil))
    (dolist (arg args)
      (let* ((start   (max (point-min) (- (point) (length arg))))
             (stop    (max (point-min) (point)))
             (content (buffer-substring-no-properties start stop)))
        (setq result (or result
                         (equal arg content)))))
    result))

(defun org-table-beginning-of-field-dwim (&optional n)
  "Moves to the beginning of the current cell, and does not move to the previous
   one if already at start."
  (interactive "p")

  (if (not (looking-back-at-p '("|" "| ")))
      (org-table-beginning-of-field 0)))

(defun my-org-select-field (&optional n)
  "Marks the contents of the current cell if in a org-mode table."
  (interactive "p")

  (org-table-beginning-of-field-dwim 0)
  (set-mark-command nil)
  (org-table-end-of-field 0))

(defmacro with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.
A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (body   `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (prog1 ,body
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (when (> elapsed 0.001)
             (message "%s... done (%.5fs)" ,title elapsed)))))))

;; we must load original helm-imenu to get access to its state-variables and matchers.
(ignore-errors
  (require 'helm-imenu))
(defun helm-imenu-dwim ()
  "Preconfigured `helm' for `imenu'. Unlike regular `helm-imenu' does always cause a helm popup."
  (interactive)
  (unless helm-source-imenu
    (setq helm-source-imenu
          (helm-make-source "Imenu" 'helm-imenu-source
            :fuzzy-match helm-imenu-fuzzy-match)))
  (helm :sources 'helm-source-imenu
        ;;:default (list (concat "\\_<" str "\\_>") str)
        :candidate-number-limit 9999
        :buffer "*helm imenu*"))

;; utility functions for key-definitions
(defun fkt (func target keys)
  "Sets up multiple keybindings for one function."
  (dolist (key keys)
    ;; dont call KBD directly. that's going to fail, at least in emacs23
    ;; ref http://stackoverflow.com/questions/7549628/whats-wrong-with-the-following-unbind-script
    (funcall func (read-kbd-macro key) target)))

;; diff-navigation

(defvar my-find-line-with-face-last-value nil)

(defun my-find-line-with-face (arg)
  "Search for the first upcoming line with a specific face.

Searches for last face, or new face if invoked with prefix-argument"
  (interactive "P")

  ;; when ARG = t, or LAST = nil
  ;; ask for new input value!
  (when (or arg
            (eq nil my-find-line-with-face-last-value))
    (setq my-find-line-with-face-last-value
          (list (read-face-name "Face to search for"))))

  (forward-line 1)
  (while (not (member (face-at-point)
                      my-find-line-with-face-last-value))
    (next-line 1)))

(defun my-ediff-find-missing ()
  (interactive)
  (let ((my-find-line-with-face-last-value '(ediff-even-diff-A
                                             ediff-even-diff-B
                                             ediff-even-diff-C)))
    (my-find-line-with-face)))

(defun save-buffer-no-hooks ()
  "Save the current buffer with all hooks disabled."
  (interactive)
  (let ((before-save-hook))
    (save-buffer)))

;; keyboard configuration

(defun lsk (target &rest keys)
  "Sets up a keymap local keybinding for target with all keys provided."
  (fkt 'local-set-key target keys))

(defun gsk (target &rest keys)
  "Sets up a global keybinding for target with all keys provided."
  (fkt 'global-set-key target keys))

;; fix next and previous-error behaviour
(gsk 'previous-error-dwim "<f7>")
(gsk 'next-error-dwim     "<f8>")
;; sometimes we want things to error (like with keyboard macros)
(gsk 'previous-error      "<S-f7>")
(gsk 'next-error          "<S-f8>")

(gsk 'beginning-of-buffer "<C-prior>")
(gsk 'end-of-buffer       "<C-next>")

(gsk 'my-switch-to-window-by-buffer-name "C-x C-b")

;; we want projectile everywhere.
(ignore-errors
  (projectile-global-mode t)
  (gsk 'projectile-find-file "C-c C-p C-f" "C-c C-p f" "C-c p f")
  (gsk 'projectile-switch-project "C-c C-p C-p" "C-c C-p p" "C-c p p"))


;;;; GLOBAL KEYBOARD DEFINITIONS


;; dont freeze emacs on ctrl-z
(global-unset-key (kbd "C-z"))

;; if C-SPC doesn't work in X, it can be because ibus is "stealing" it.
;; reconfigure ibus with ibus-setup.

;; we know these ones from everywhere else
(gsk 'undo         "C-z")
(gsk 'other-window "<C-tab>")

(gsk 'yank-quote-dwim "C-M-y") ;; sexps yank.

;; C-x k is bound to kill, but C-x C-k is bound to nothing
;; we hit this all the time, so bind it.
(gsk 'ido-kill-buffer "C-x C-k")

;; make meta-j join the following line with the current one
(gsk 'my-join-line-with-next "M-j")

;; align-regexp lets you align stuff based on match
;; ab (bc)
;; bcdhskjfdhskj (dc)
;;
;; to align by second column, M-x align-regex (
(gsk 'align-regexp "M-&")

;; search for all matches of a given regex, list results
(gsk 'occur-dwim "C-c C-o" "M-s o" "M-s M-o")

;; upcase and downcase like we want it
(gsk 'upcase-word-dwim "M-u")
(gsk 'downcase-word-dwim "M-l")

;; rgrep is grep for emacs
(gsk 'rgrep "C-c C-g")

(gsk 'magit-status "C-x v g")
(gsk 'git-timemachine "C-x v h")

;; general text-completion. enable everywhere.
;; improve it with this setup here:
;; http://ianeslick.com/2013/05/17/clojure-debugging-13-emacs-nrepl-and-ritz/
(gsk 'hippie-expand "C-." "C-:") ;;(gsk 'dabbrev-expand "C-.")

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
(gsk 'er/expand-region "C-+" "C-?" "M-+") ;; ? = S-? => C-S-+ = C-?

;; ace jump - remember C-u C-c SPC etc
;;(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; undo-tree - enable globally
(ignore-errors
  (global-undo-tree-mode 1)
  (gsk 'undo-tree-redo "C-M-z") ;; quick access to redo.
  )
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

(gsk #'my-find-line-with-face "<f9>")
(gsk #'my-ediff-find-missing "<s-f9>")


;; compilation-mode tweaks:

;; remove association for guile-files. the reason for this is that the guile-compiler
;; emits errors in the same form as emacs does when byte-compiling.
;; compilation-mode will this "hit" on lines like these:
;; In toplevel form:
;; In end of data:
;; It will however not have a file match and break prev/next-error navigation.
(ignore-errors
  (assq-delete-all 'guile-file compilation-error-regexp-alist-alist))
;; found using the elisp below.
;; (let* ((result nil))
;;   (dolist (item compilation-error-regexp-alist-alist)
;;     (let* ((category (car item))
;;            (regexp   (car (cdr item))))
;;       (setq result (concat result "\n" (symbol-name category) ": " regexp))))
;;   result)


;; spell-checking via languagetool
(let* ((jar (expand-file-name "~/build/LanguageTool-3.0/languagetool-commandline.jar")))
  ;; TODO: auto-download ZIP from https://languagetool.org/download/LanguageTool-3.0.zip
  ;; and uncompress to get jar without manually having to download this.
  (when (file-exists-p jar)
    (ignore-errors
      (require 'langtool)
      (setq langtool-language-tool-jar jar
            langtool-mother-tongue "en"
            langtool-disabled-rules '("WHITESPACE_RULE"
                                      "EN_UNPAIRED_BRACKETS"
                                      "COMMA_PARENTHESIS_WHITESPACE"
                                      "EN_QUOTES")))))

(defun langtool-check-buffer-dwim (arg)
  "Check and correct buffer with langtool."
  (interactive "p")

  (if (= 1 arg)
      ;; default, no prefix argument.
      (langtool-check-buffer)
    ;; non-default. prefix-argument. C-u or something.
    (langtool-correct-buffer)))

;; prevent accidentally enabling overwrite-mode
(defun overwrite-mode-prompt ()
  "A wrapper to ensure overwrite-mode is never enabled blindly."
  (interactive)
  (if (not (is-mode-active-p 'overwrite-mode))
      (when (yes-or-no-p "WARNING! Enabling overwrite-mode. Please confirm")
        (overwrite-mode 't))
    (overwrite-mode 0)))

(gsk 'overwrite-mode-prompt "<insertchar>" "<insert>")

(gsk 'elfeed "C-x w")

;; build should be available everywhere!
(gsk 'compile "<f5>")
(gsk 'recompile "<C-f5>")

;;;; MODE CUSTOMIZATIONS


;; performance
(add-hook 'focus-out-hook #'garbage-collect)

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
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t)

  ;; many macros start with def-something. We want those usages in imenu.
  (add-to-list 'imenu-generic-expression
               '(nil "^\\s-*(def\\sw+\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 1 ) t)

  ;; many macros start with def-something. We want those usages in imenu.
  (add-to-list 'imenu-generic-expression
               '(nil "^\\s-*(ert-deftest\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 1 ) t)

  (lsk 'macrostep-expand "C-c C-e")
  (lsk 'eval-buffer "C-c C-c")

  (lsk #'my-ert-run-tests "<C-f5>")

  ;; enable intelligent navigation with M-, and M-.
  (elisp-slime-nav-mode)

  ;; we want documentation
  (eldoc-mode t))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'ielm-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; common lisp
(defhook lisp-mode-hook
  ;; we know this one from VS :)
  ;; we can also check symbols in slime with M-.
  (lsk 'slime "<f5>")
  (paredit-mode))

;; paredit
(defhook paredit-mode-hook
  ;; editing. keybindings which makes sense AND whic works in SSH
  (lsk 'paredit-forward-slurp-sexp  "<C-left>"   "M-[ c")
  (lsk 'paredit-forward-barf-sexp   "<C-right>"   "M-[ d")
  (lsk 'paredit-backward-slurp-sexp "<C-M-left>"  "ESC M-[ c")
  (lsk 'paredit-backward-barf-sexp  "<C-M-right>" "ESC M-[ d")

  ;; navigation
  (lsk 'paredit-backward            "<C-M-up>"    "ESC M-[ A")
  (lsk 'paredit-forward             "<C-M-down>"  "ESC M-[ B")

  (lsk 'paredit-splice-sexp          "C-M-s")
  (lsk 'paredit-split-sexp          "C-M-S")
  ;; should be set by default, but gets overriden by over global-set-key
  (lsk 'paredit-join-sexps          "M-J")
  (lsk 'my-join-line-with-next      "M-j")

  (show-paren-mode 1) ; turn on paren match highlighting
  ;;(setq show-paren-style 'expression) ; highlight entire bracket expression
  )

;; clojure
(defhook clojure-mode-hook
  ;; key-bindings
  (lsk 'nrepl-jack-in "<f5>") ; because we know this from VS!

  ;; settings
  (paredit-mode)
  ;; for clojure comments - override annoying elisp mode single-; comment-indentation.
  (setq comment-column 0))

;; haskell
(defhook haskell-mode-hook
  ;; we want proper indentation
  (haskell-indent-mode +1))

;; ;; c/c++
;; (defun my-c-mode-hook ()
;;   ;; required for auto-completion
;;   (semantic-mode 1))
;; (add-hook 'c-mode-hook 'my-c-mode-hook)
;; (add-hook 'c++-mode-hook 'my-c-mode-hook)

;; C# is better with omnisharp, if available
(defhook csharp-mode-hook
  ;; hide-show is nice for modes which supports it.
  ;; (hs-minor-mode)
  (local-unset-key (kbd "M-m"))
  (lsk 'hs-toggle-hiding "M-m M-m")

  ;; cc-mode overrides some of our global bindings.
  ;; search for all matches of a given regex, list results
  (lsk 'occur-dwim "C-c C-o" "M-s o" "M-s M-o")
  (lsk 'helm-imenu "<f12>")

  (when my-enable-omnisharp
    (progn
      ;; company mode must be configured before omnisharp is loaded.
      (eval-after-load 'company
        '(add-to-list 'company-backends 'company-omnisharp))

      ;; prepare imenu-support
      (setq omnisharp-imenu-support t)
      (omnisharp-mode t)
      (omnisharp-imenu-create-index)
      ;; disable csharp-mode imenu when omnisharp is running!
      (setq csharp-want-imenu nil)

      ;; vs/resharper-like bindings
      (lsk 'omnisharp-helm-find-usages "S-<f12>")
      (lsk 'omnisharp-find-implementations-popup "M-<f11>")
      ;; C-r is taken for reverse isearch, so we do C-o for omnisharp
      (local-unset-key (kbd "C-o"))
      (lsk 'omnisharp-rename "C-o C-r")
      (lsk 'omnisharp-rename-interactively "C-u C-o C-r")

      ;; type-info
      (lsk 'omnisharp-current-type-information "C-c C-t")

      ;; overrides
      (lsk 'omnisharp-go-to-definition "<f12>" "M-.") ;; like cslisp smart-navn
      (lsk 'pop-tag-mark "M-,")
      (lsk 'omnisharp-auto-complete "C-.") ;; override company-mode, with better popup
      (lsk 'hippie-expand "C-:") ;; still allow hippie-expand

      (lsk #'omnisharp-run-code-action-refactoring "C-<return>" "C-M-<return>" "M-<return>")

      )))

(defhook c-mode-common-hook
  ;; setup navigation based on tags.
  (lsk 'find-tag "<f12>" "M-.")
  (lsk 'pop-tag-mark "M-,")

  (when (derived-mode-p 'c-mode 'c++-mode)
    (ignore-errors
      (require 'rtags)
      (lsk 'rtags-find-symbol-at-point "<f12>" "M-.")
      (lsk 'rtags-find-references-at-point "S-<f12>"))))

(defhook js2-mode-hook
  (lsk 'run-js "<f6>")
  (lsk 'js-send-region "C-x C-e")

  (lsk 'my-join-line-with-next "M-j")

  (lsk 'occur-dwim "C-c C-o")

  ;; nodejs stuff
  (lsk 'nodejs-repl "C-c C-r")
  (lsk 'my-nodejs-send-region-to-repl "C-c C-e")
  (lsk 'my-nodejs-eval-buffer "C-c C-c"))

(defhook python-mode-hook
  ;; elpy improves python-coding considerably, when on a
  ;; well-supported platform, so package not installed by default.
  (ignore-errors
    (require 'elpy))
  (when (fboundp 'elpy-mode)
    (let* ((python-exe (if (eq system-type 'windows-nt)
                           "python.exe" ;; python3 is called python on windows!
                         "python3"))
           (pdb (concat python-exe " -m pdb")))

      (setq elpy-rpc-python-command python-exe)
      (elpy-use-cpython python-exe)
      (setq python-shell-interpreter python-exe)

      (elpy-mode)
      (define-key elpy-mode-map (kbd "C-c o") #'occur-dwim)
      (define-key elpy-mode-map (kbd "C-c C-o") #'occur-dwim)

      ;; gud/realgud defaults to a seperate pdb executable which does not
      ;; exist on Fedora. Just use python and pdb module directly.
      (setq gud-pdb-command-name pdb)
      (setq realgud:pdb-command-name pdb))
    (lsk 'realgud:pdb "C-<f5>")))

(ignore-errors
  (elpy-enable))

;; rust
(defhook rust-mode-hook
  (require 'lsp-mode)
  (lsp)
  (cargo-minor-mode)
  (eldoc-mode)
  (company-mode)

  (lsk 'company-indent-or-complete-common "TAB"))

;; lsp
(defhook lsp-mode
  (lsk 'lsp-rename "C-c C-r"))


;; typescript
(defhook typescript-mode-hook
  ;; TIDE
  ;; explicitly null preference to enable auto-detection.
  (setq tide-tsserver-executable nil)
  (ignore-errors
    (tide-setup)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)

    ;; format options
    (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

    (lsk #'tide-references "S-<f12>")
    (lsk #'tide-rename-symbol "C-c C-r" "C-c r")
    (lsk #'tide-documentation-at-point "C-c C-d" "C-c d")

    ;; ts-comint
    (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
    (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
    (local-set-key (kbd "C-c b") 'ts-send-buffer)
    (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
    (local-set-key (kbd "C-c l") 'ts-load-file-and-go)
    (lsk #'ts-send-buffer "C-c C-c")
    (lsk #'ts-send-last-sexp "C-x C-e" "C-x e" "C-M-x")
    (lsk #'ts-load-file "C-c l")
    (lsk #'hippie-expand "C-:")
    (lsk #'tide-fix "C-<return>" "C-M-<return>" "M-<return>")
    (lsk #'tide-nav "C-M-t")
    (lsk #'tide-jump-to-implementation "C-M-.")

    ;; have easy access to tsc, always.
    (lsk #'my-ts-tsc "<C-S-f5>")))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; org-mode
(defhook org-mode-hook
  ;; keybindings
  (lsk 'org-store-link "C-c l")
  (lsk 'org-agenda     "C-c a")
  (lsk 'org-iswitchb   "C-c b")
  ;; override C-c ¨ as that doesnt work on norwegian keyboards
  (lsk 'org-table-sort-lines "C-c s")

  ;; override some defaults
  (lsk 'org-table-beginning-of-field-dwim "M-a")
  ;; TODO: create similar  end-of-field-dwim and map to M-e for symmetry.

  ;; select the contents of a cell.
  (lsk 'my-org-select-field "C-M-+")

  ;; edit source in org-babel, org-tables etc. default mapped to only "C-c '"
  (lsk 'org-edit-special "C-c C-'")

  ;; we want proper new-line indentation
  ;;(lsk 'org-return-and-indent "RET")

  ;; enable imenu, and add standard navigation.
  (imenu-add-menubar-index)
  (lsk 'helm-imenu-dwim "M-g m" "M-g M-m" "M-g f" "M-g M-f"))

(defhook org-src-mode-hook
  ;; create easy exit from org-edit-special.
  (lsk 'org-edit-src-exit "C-c C-'"))

(defhook prog-mode-hook
  ;; keybindings

  ;; not C-k C-c & C-k C-t because C-k is kill-line in emacs
  (lsk 'comment-or-uncomment-region  "C-;" "C-M-,")
  (lsk 'uncomment-region             "C-:")

  ;; code - navigate to definition
  (lsk 'imenu-nav-dwim "<f12>")
  (lsk 'highlight-symbol-occur "S-<f12>")

  (lsk 'helm-imenu-dwim "M-g m" "M-g M-m" "M-g f" "M-g M-f")
  (lsk 'helm-imenu-anywhere "C-M-g C-M-m" "C-M-g C-M-f")
  ;; navigate back again.
  ;; (could also use set-mark with prefix argument C-u C-spc.)
  (lsk 'pop-local-or-global-mark "C--")

  ;; auto-complete is a must
  (company-mode 1)

  ;; settings
  ;; ensure we have line-numbers in all code-files
  ;;(global-linum-mode 1)
  (linum-mode 1)

  ;; highlight current function?
  (which-function-mode 1)

  ;; flashy flashy
  (highlight-symbol-mode 1)

  ;; enable imenu - only for true prog-mode major-modes
  (when (derived-mode-p 'prog-mode)
    (ignore-errors
      (imenu-add-menubar-index)))

  (setq-local fill-column 80)
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode t)

  ;; formatting matters in programming files, but python is a silly
  ;; language which cares about white-space.
  ;; also: for any non-lisp (paredit) language, enable electric-pair-mode.
  (if (is-lisp-p)
      (progn
        (lsk 'indent-whole-buffer "C-i")
        (lsk 'comment-or-uncomment-sexp "M-;" "C-M-;"))
    (progn
      (if (fboundp #'electric-pair-local-mode)
          (electric-pair-local-mode 1)
        (electric-pair-mode 1))))

  ;; flycheck is super-useful
  (flycheck-mode t)

  ;; flyspell too!
  ;; (my-enable-flyspell-mode t)

  ;; must be set AFTER flyspell!
  (lsk 'company-complete "C-.")

  ;; realgud needs to be required
  (require 'realgud)
  ;; allow variable inspection on right-mouse click!
  (define-key realgud:shortkey-mode-map [mouse-3] #'realgud:tooltip-eval)

  ;;(require 'yasnippet)
  ;;(yas-minor-mode-on)
  )

(add-hook 'powershell-mode-hook 'my-prog-mode-hook)
(add-hook 'css-mode-hook 'my-prog-mode-hook)
(add-hook 'cmake-mode-hook 'my-prog-mode-hook)

;; xml
(defhook nxml-mode-hook
  (lsk 'comment-or-uncomment-region "C-c C-c")
  (lsk 'uncomment-region            "C-c C-u")

  (lsk 'nxml-where                  "C-c C-w")
  (lsk 'nxml-pretty-print-buffer    "C-c C-e")

  ;; causes entire elements (with children) to be treated as sexps.
  (setq nxml-sexp-element-flag t)

  ;; C-c C-o is bound to some useless heading/section things
  ;; we're not using. Restablish occur-dwim as a "global" binding.
  (local-unset-key (kbd "C-c C-o"))
  (local-unset-key (kbd "C-c"))
  (local-unset-key (kbd "C"))
  (local-set-key (kbd "C-c C-o") #'occur-dwim)

  ;; xml-files are more often than not part of a project.
  (projectile-mode t)

  ;; autofill mode should NEVER be on for xml!
  (auto-fill-mode 0))



;; html/web
(defhook web-mode-hook
  (lsk 'web-mode-comment-or-uncomment "C-c C-c")
  (lsk 'web-mode-uncomment            "C-c C-u")

  (when (or (string-suffix-p (buffer-file-name) ".js")
            (string-suffix-p (buffer-file-name) ".json"))
    (my-js-mode-hook)))

;; css should be prog-mode but isn't
(add-hook 'css-mode-hook 'my-prog-mode-hook)

;; things like markdown
(defhook text-mode-hook
  (my-enable-flyspell-mode nil)

  ;; make line-wraps where they should be according to
  ;; ancient conventions.
  (auto-fill-mode t)

  (lsk 'langtool-check-buffer-dwim "C-c C-l")
  (lsk 'langtool-show-message-at-point "C-c C-p")
  (lsk 'flyspell-correct-word-before-point "C-c C-k")
  (lsk 'langtool-goto-previous-error "<f7>")
  (lsk 'langtool-goto-next-error "<f8>"))

(defhook markdown-mode-hook
  (lsk 'helm-imenu-dwim "M-g m" "M-g M-m" "M-g f" "M-g M-f")
  (lsk 'helm-imenu-anywhere "C-M-g C-M-m" "C-M-g C-M-f"))

;; git commits are text too.
(add-hook 'git-commit-mode-hook 'my-text-mode-hook)

;; eww thingies
(defhook eww-mode-hook
  (lsk 'eww-readable "C-c C-r" "C-c r")
  (lsk 'my-eww-reflow "M-g" "M-g"))

(defun eww-dgg ()
  "Search the internets using DDG with `eww'."
  (interactive)
  (eww (concat "https://duckduckgo.com/html/?q=" (url-encode-url (read-from-minibuffer "Search for: ")))))

;; dired
(defhook dired-mode-hook
  ;; move cursor to beginning of filename when that makes sense
  (setq wdired-use-dired-vertical-movement 'sometimes)

  ;; improves defaults when moving or copying across dired-buffers.
  ;; this of it as norton commander for Emacs.
  (setq dired-dwim-target t)

  (lsk 'dired-isearch-filenames "C-s")
  (lsk 'isearch-forward "C-S"))

;; shellstuff
(defhook shell-mode-hook
  (compilation-shell-minor-mode)
  (company-mode)
  (lsk #'company-manual-begin "\t"))

;;;; WINDOWS ONLY CUSTOMIZATIONS


(defun my-windows-mode-hook ()
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
  (setq ediff-diff3-program "C:\\cygwin64\\bin\\diff3.exe")

  ;; requires aspell installed by cygwin
  (setq-default ispell-program-name "C:/cygwin64/bin/aspell.exe")

  ;; load tfs-support if tfs.el is installed.
  (ignore-errors
    (require 'tfs)
    (setq tfs/tf-exe  "C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\Common7\\IDE\\tf.exe")
    (setq tfs/tfpt-exe "C:\\Program Files (x86)\\Microsoft Team Foundation Server 2013 Power Tools\\TFPT.EXE")
    (setq tfs/login ""))

  ;; omnisharp
  ;; (setq omnisharp-server-executable-path "D:/Git/omnisharp-roslyn/scripts/Omnisharp.cmd")
  )

;;;; UNIX ONLY CUSTOMIZATIONS


(defun my-unix-mode-hook ()
  ;; allow us to (automatically) open files as root when needed via tramp and /sudo::
  (require 'tramp)
  ;; (defadvice find-file (after find-file-sudo activate)
  ;;   "Find file as root if necessary."
  ;;   (unless (and buffer-file-name
  ;;                (file-writable-p buffer-file-name))
  ;;     (find-alternate-file (concat "/sudo::" buffer-file-name))))

  ;; hunspell is supposedly better and more modern than ispell.
  ;; use it when available.
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t))

  (when (executable-find "python3")
    ;; (setq python-shell-interpreter "python3")
    (setq elpy-rpc-python-command "python3"))

  ;; StumpWM/Common-lisp related stuff
  ;; Install using sbcl.
  ;; (ql:quickload "quicklisp-slime-helper")
  (let ((file-name (expand-file-name "~/quicklisp/slime-helper.el")))
    (when (file-exists-p file-name)
      (progn
        (load file-name)
        (setq inferior-lisp-program "sbcl")
        (require 'slime-autoloads)
        (require 'slime-company)
        (slime-setup '(slime-fancy slime-asdf slime-company)))))

  ;; omnisharp
  (setq omnisharp-server-executable-path "/home/jostein/build/omnisharp-roslyn/omnisharp.sh"))

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


(defun my-x-mode-hook ()


  ;; make keys act immediately
  ;; http://unix.stackexchange.com/questions/28170/some-keys-are-invalid-on-emacs-when-using-german-keyboard
  (define-key key-translation-map [dead-grave] "`")
  (define-key key-translation-map [dead-acute] "'")
  (define-key key-translation-map [dead-circumflex] "^")
  (define-key key-translation-map [dead-diaeresis] "\"")
  (define-key key-translation-map [dead-tilde] "~")
  (define-key isearch-mode-map [dead-grave] nil)
  (define-key isearch-mode-map [dead-acute] nil)
  (define-key isearch-mode-map [dead-circumflex] nil)
  (define-key isearch-mode-map [dead-diaeresis] nil)
  (define-key isearch-mode-map [dead-tilde] nil))

;; special workaround for dead keys needed only when running emacs in Linux & X.
(when (and (display-graphic-p)
           (eq system-type 'gnu/linux))
  (my-x-mode-hook))

;; un-disabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; configure company-mode
(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-c-headers)
     (add-to-list 'company-backends 'company-web-html)))

(global-ede-mode 1)

;; enable eww as the main browser
(ignore-errors
  (require 'eww))

;; if wa want links cliked in elfeed and friends to open in emacs,
;; we must tell emacss to use eww.
(setq browse-url-browser-function 'eww-browse-url)
;;(setq browse-url-browser-function 'browse-url-default-browser)

;; unless built from emacs master, we may encounter some errors in 24.4
;; See: https://emacs.stackexchange.com/questions/5469/invalid-date-01-jan-2055

;; Override built in function with implementation in master to fix it in all versions.
(defun url-cookie-expired-p (cookie)
  "Return non-nil if COOKIE is expired."
  (let ((exp (url-cookie-expires cookie)))
    (and (> (length exp) 0)
         (condition-case ()
             (> (float-time) (float-time (date-to-time exp)))
           (error nil)))))

(defun my-strip-whitespace (string)
  "Remove excess white spaces in beginning, ending and middle of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "[ \t\n]+" " "
                            (replace-regexp-in-string "\\`[ \t\n]*" ""
                                                      (replace-regexp-in-string "[ \t\n]*\\'" "" string))))

(defun my-eww-get-title ()
  "Version-independent title-extractor for eww."
  (if (boundp 'eww-data)
      (plist-get eww-data :title)
    eww-current-title))

(defun my-eww-get-url ()
  "Version-independent url-extractor for eww."
  (if (boundp 'eww-data)
      (plist-get eww-data :url)
    eww-current-url))

;; Fix eww's one-buffer-only behaviour
(defun my-set-eww-buffer-title ()
  "Set the title of the current eww-buffer based on the current web-page.

  A side effect of this is that the buffer is made unique
  and wont be replaced by other eww-invocations."
  (let* ((title  (my-strip-whitespace (my-eww-get-title)))
         (url    (my-eww-get-url))
         (result (concat "*eww-" (or title
                                     (if (string-match "://" url)
                                         (substring url (match-beginning 0))
                                       url)) "*")))
    (rename-buffer result t)))


;; support emacs 24.5 and friends as far as possible
(if (boundp 'eww-after-render-hook)
    (add-hook 'eww-after-render-hook 'my-set-eww-buffer-title)

  (defadvice eww-render (after kill-buffer activate)
    (my-set-eww-buffer-title)))




(defun my-eww-reflow ()
  "Causes the current buffer to reflow if it's a eww-buffer."
  (interactive)
  (when (string= "eww-mode" major-mode)
    (eww-reload t nil)))


;; how to remap a key for function X to function Y.
;; (define-key irony-mode-map [remap completion-at-point]
;;   'irony-completion-at-point-async)


;; patch yaml-mode syntax table to ignore single-quotes:
;; fix formatting error.
(setq yaml-mode-syntax-table
      (let ((syntax-table (make-syntax-table)))
        ;; (modify-syntax-entry ?\' "\"" syntax-table)
        (modify-syntax-entry ?\" "\"" syntax-table)
        (modify-syntax-entry ?# "<" syntax-table)
        (modify-syntax-entry ?\n ">" syntax-table)
        (modify-syntax-entry ?\\ "\\" syntax-table)
        (modify-syntax-entry ?- "_" syntax-table)
        (modify-syntax-entry ?_ "_" syntax-table)
        (modify-syntax-entry ?& "." syntax-table)
        (modify-syntax-entry ?* "." syntax-table)
        (modify-syntax-entry ?\( "." syntax-table)
        (modify-syntax-entry ?\) "." syntax-table)
        (modify-syntax-entry ?\{ "(}" syntax-table)
        (modify-syntax-entry ?\} "){" syntax-table)
        (modify-syntax-entry ?\[ "(]" syntax-table)
        (modify-syntax-entry ?\] ")[" syntax-table)
        syntax-table))
