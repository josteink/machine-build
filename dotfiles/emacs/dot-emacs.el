
;;;; Custom

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

;;;; PACKAGES AND REPOSITORIES

;;
;; header supposedly required for use-package
;;
(require 'package)
(add-to-list 'package-archives '("gnu"      . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa"    . "https://melpa.org/packages/"))
(package-initialize)

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-always-ensure t
        use-package-expand-minimally t))

;;;; early gui customization (cause less flicker)

;; probe first to not crash on emacs-nox
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(defun my-get-arp-table ()
  "Gets the ARP table for the current system."
  (cond ((eq system-type 'windows-nt)
         (let ((arp (shell-command-to-string "arp -a")))
           arp))
        ((eq system-type 'gnu/linux)
         (use-package f :ensure t)
         (require 'f)
         (let ((arp (f-read-text "/proc/net/arp")))
           arp))
        ((eq system-type 'darwin)
         (let ((arp (shell-command-to-string "arp -an")))
           arp))))

(defun at-home-network-p ()
  (let ((arp (my-get-arp-table)))
    (string-match-p "24:f5:a2:23:f0:33" arp)))

;; activate theme early!
(use-package dracula-theme
  :ensure t
  :config
  (progn
    (load-theme 'dracula t)
    (set-face-foreground 'font-lock-comment-delimiter-face "#ff8040")
    (when (not (at-home-network-p))
      ;; increase comment contrast when at the office, because of sunlight
      (set-face-foreground 'font-lock-comment-face "#9988bb")
      ;; make selected text more obviously selected
      (set-face-background 'region "#44677a")
      )))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; used by doom modeline!
(use-package all-the-icons
  :ensure t
  :init
  (when (not (or
              (file-exists-p (expand-file-name "~/.local/share/fonts"))
              (file-exists-p (expand-file-name "~/Library/Fonts"))))
    (all-the-icons-install-fonts)))

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

;;;; GUI specific configuration

(defun my-gui-mode-hook ()
  ;; smooth scrolling
  (pixel-scroll-precision-mode t)

  ;; only activate global-line mode when on X11/windows/non-terminal environment.
  ;; will deactivate syntax highlighting and more in SSH.
  (global-hl-line-mode +1)

  ;; same with column-numbers.
  (column-number-mode +1)

  (let ((size (if (at-home-network-p) 13 13)))
    ;; font thingie, downloaded from http://sourcefoundry.org/hack/
    (try-set-default-font "Monaspace Neon Var" size)))

(when (display-graphic-p)
  (my-gui-mode-hook))

;; support frames created by emacsclients when Emacs is launched as a
;; persistent daemon.
(defun my-after-create-frame-function (frame)
  (my-gui-mode-hook))
(add-hook 'after-make-frame-functions #'my-after-create-frame-function)


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


;;;; Define global behaviours

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode t)
  :bind ("C-M-z" . undo-tree-redo))

;; no need to rely on IDO for this!
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

(use-package nlinum
  :ensure t
  :config (global-nlinum-mode t))

(use-package multiple-cursors
  :bind (;; puts a cursor on everyline of a selected region.
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

(use-package expand-region
  :bind (("C-+" . er/expand-region)
         ("C-?" . er/expand-region)
         ("M-+" . er/expand-region)))

;; main completion framework, like helm.
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 't))

;; provides more information about matches in completion
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 't))

;; provides nice general navigation
(use-package consult
  :ensure t
  :after vertico
  :bind (
         ("C-x b" . consult-buffer)
         ("C-c g" . git-grep-dwim)
         ("<f7>"  . vertico-previous)
         ("<f8>"  . vertico-next)))

;; provides nice icons in completion
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; enables fuzzy matching in completion
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(basic substring initials flex orderless))
  (completion-category-overrides '((file (styles basic partial-completion)))))
;; flex is too flex?
;; (setq orderless-matching-styles '(orderless-flex))
(setq orderless-matching-styles '(orderless-literal))

;; allow us to export consult-grep etc to its own grep-buffer
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))
(use-package embark-consult
  :ensure t)

(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

(use-package magit
  :ensure t
  :bind ("C-x v g" . magit-status))

(use-package highlight-symbol
  :ensure t
  :hook (prog-mode . highlight-symbol-mode))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package editorconfig
  :ensure t
  :hook (prog-mode . editorconfig-mode))

;; elisp-slime-nav elisp-refs
(use-package elisp-slime-nav
  :ensure t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; ensure we activate virtualenvs in python projects managed
;; by poetry
(use-package poetry
  :ensure t
  :config (poetry-tracking-mode t))

;;;; Setting up modes and file-mappings


(defun add-extensions-to-mode (mode &rest extensions)
  "Register the provided `extensions' to handle the provided `mode'."
  (dolist (extension extensions)
    (add-regexp-to-mode mode (concat "\\." extension "$"))))

(defun add-regexp-to-mode (mode &rest patterns)
  "Register the provided `patterns' to handle the provided `mode'."
  (dolist (rx patterns)
    ;; dont add at the end, add at tyhe beginning!
    ;; (ie override defaults, if present!)
    (setq auto-mode-alist
          (cons
           (cons rx mode)
           auto-mode-alist))))


;; built-in to Emacs, no need to "ensure", but we want to use the tree-sitter versions!
(setq major-mode-remap-alist
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (c-or-c++-mode   . c-or-c++-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (css-mode        . css-ts-mode)
        (csharp-mode     . csharp-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-json-mode    . json-ts-mode)
        (python-mode     . python-ts-mode)
        (sh-mode         . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (java-mode       . java-ts-mode)
        (go-mode         . go-ts-mode)
        (rust-mode       . rust-ts-mode)
        (yaml-mode       . yaml-ts-mode)))

(add-extensions-to-mode 'tsx-ts-mode "tsx")


;; default is level 3, which is not as advanced/nice.
(setq-default treesit-font-lock-level 4)

;; can't be added with use-package, but is emacs-internal anyway!
(add-extensions-to-mode 'nxml-mode "config" "merge" ".*proj" "xaml" "props" "resx") ;; .NET, SuperOffice config-merge.
(add-extensions-to-mode 'html-mode "html" "php" "ascx" "aspx" "cshtml")
(add-extensions-to-mode 'message-mode "somail" "eml")

;; MELPA modules
(use-package bmx-mode      :defer t :hook bat-mode)
(use-package cargo         :defer t :hook (rust-ts-mode . cargo-minor-mode))
(use-package cmake-mode    :defer t :mode "CMakeLists.txt")
(use-package crontab-mode  :defer t :mode "crontab")
(use-package markdown-mode :defer t :mode "\\.md\\'")
(use-package powershell    :defer t :mode ("\\.psm?1\\'" . powershell-mode))
(use-package dockerfile-ts-mode :defer t :mode "[dD]ockerfile$")
(use-package wsd-mode      :defer t :mode "\\.wsd\\'")

;; prog-mode customizations
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode       . paredit-mode)
         (clojure-mode    . paredit-mode)
         (lisp-data-mode  . paredit-mode)))

(use-package combobulate
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (html-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)
         (toml-ts-mode . combobulate-mode))
  :vc ( :url "https://github.com/mickeynp/combobulate"
        :rev :newest)
  :config
  (progn
    (define-key combobulate-key-map (kbd "M-p") #'combobulate-drag-up)
    (define-key combobulate-key-map (kbd "M-n") #'combobulate-drag-down))
  )

;; copilot
(use-package copilot-mode
  :defer t
  :vc ( :url "https://github.com/copilot-emacs/copilot.el"
        :rev :newest)
  :config
  (progn
    (require 'copilot)
    (add-to-list 'copilot-major-mode-alist '("csharp-ts" . "csharp"))
    (add-to-list 'copilot-major-mode-alist '("typescript-ts" . "typescript"))
    (add-to-list 'copilot-major-mode-alist '("tsx-ts" . "typescriptreact"))

    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

    (add-hook 'prog-mode-hook (lambda ()
                                (ignore-errors
                                  (copilot-mode))
                                ))))

;; configure major-mode agnostic packages

;; DAP/debug support
(use-package dap-mode
  :defer t
  :commands (dap-start-debugging)
  :config
  (progn
    ;; dap/debug support
    (require 'dap-mode)
    (require 'dap-python)
    (require 'dap-pwsh)
    (require 'dap-node)
    (require 'dap-netcore)
    (require 'dap-gdb-lldb)
    (dap-register-debug-template "Rust::GDB Run Configuration"
                                 (list :type "gdb"
                                       :request "launch"
                                       :name "GDB::Run"
                                       :gdbpath "rust-gdb"
                                       :target nil
                                       :cwd nil))
    (dap-auto-configure-mode 1)))

(use-package indent-bars
  :vc ( :url "https://github.com/jdtsmith/indent-bars"
        :rev :newest)
  :config
  (require 'indent-bars-ts)             ; not needed with straight
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope
   '(
     (python function_definition class_definition for_statement
             if_statement with_statement while_statement)
     (typescript function_declaration class_declaration method_definition
                 try_statement if_statement for_statement for_in_statement while_statement
                 statement_block)
     (tsx function_declaration class_declaration method_definition
          try_statement if_statement for_statement for_in_statement while_statement
          statement_block
          jsx_opening_element)
     (bicep resource_declaration object))
   )
  :hook ((prog-mode . indent-bars-mode)
         (yaml-ts-mode . indent-bars-mode)))


;; make project.el behave like projectile:
;; switch straight to file selector when switching project
(setq project-switch-commands #'project-find-file)

(setq lsp-warn-no-matched-clients nil)

;; hook js2-mode in for shell scripts running via node.js
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;;; GLOBAL DEFAULT OVERRIDES

;; do not require files to end in \n
(setq require-final-newline nil)
;; dont let other sysadmins override our config ;)
(setq inhibit-default-init t)
(require 'org)
(setq initial-buffer-choice 'org-agenda-list)

;; supress splash-screen.
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
;; its annoying always having to say "yes" to close client-opened files
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; "big files" are a thing of the past
(setq large-file-warning-threshold nil)

;; supress magit-nagging
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil)

(setq org-support-shift-select t)
(setq org-todo-keywords
      '((sequence "TODO" "ACTIVE" "|" "DONE" "DELEGATED")))

;; ensure weeks starts on mondays
(setq calendar-week-start-day 1)

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

;; treemacs child frame-reading bugs out on sway/wayland
(setq treemacs-read-string-input 'from-minibuffer)

;; enable narrowing and widening of buffers via C-x n n and C-x n w
(put 'narrow-to-region 'disabled nil)

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

;; always follow symlinks to files under source-control. dont ask.
(setq vc-follow-symlinks t)

;; UTF-8 as default encoding. this is required for unix->windows safe dropbox transfers.
(set-language-environment "UTF-8")

;; generally speaking we always want spaces, not tabs.
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; try to set indentation consistently.
(setq tab-width 4)

;; use node for JS-execution
(setq inferior-js-program-command "node --interactive")

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
  (if indent-tabs-mode
      (tabify (point-min) (point-max))
    (untabify (point-min) (point-max))))

(defun indent-whole-project (extension)
  "Indent all files in a project project, based on provided `EXTENSION'."
  (interactive "sEnter file-extension: ")

  (let* ((project--root  (project-root (project-current)))
         (project--files (project-files project--root))
         (actual-files  (seq-filter (lambda (filename)
                                      (string-suffix-p extension filename 't)) project--files)))
    (dolist (file actual-files)
      (find-file (concat project-root file))
      (delete-trailing-whitespace)
      (indent-whole-buffer)
      (save-buffer))))

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

(defun git-grep-dwim ()
  (interactive)
  ;; same os occur-dwm... get good defaults!
  ;; all files, always
  ;; use projectile root, to get root folder automatically
  (let ((current-symbol (region-str-or-symbol)))
    (consult-grep (project-root (project-current)) current-symbol)))

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
        (save-buffer)
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

;; copy&paste from https://emacs.stackexchange.com/a/27011
(defun my/org-get-folded-state ()
  (cond
   ((not (or (org-at-item-p) (org-at-heading-p)))
    'not-at-node)
   ((org-before-first-heading-p)
    'not-at-node)
   (t
    (let (eoh eol eos has-children children-skipped struct)
      ;; First, determine end of headline (EOH), end of subtree or item
      ;; (EOS), and if item or heading has children (HAS-CHILDREN).
      (save-excursion
        (if (org-at-item-p)
            (progn
              (beginning-of-line)
              (setq struct (org-list-struct))
              (setq eoh (point-at-eol))
              (setq eos (org-list-get-item-end-before-blank (point) struct))
              (setq has-children (org-list-has-child-p (point) struct)))
          (org-back-to-heading)
          (setq eoh (save-excursion (outline-end-of-heading) (point)))
          (setq eos (save-excursion (org-end-of-subtree t t)
                                    (when (bolp) (backward-char)) (point)))
          (setq has-children
                (or (save-excursion
                      (let ((level (funcall outline-level)))
                        (outline-next-heading)
                        (and (org-at-heading-p t)
                             (> (funcall outline-level) level))))
                    (save-excursion
                      (org-list-search-forward (org-item-beginning-re) eos t)))))
        ;; Determine end invisible part of buffer (EOL)
        (beginning-of-line 2)
        (while (and (not (eobp)) ;; this is like `next-line'
                    (get-char-property (1- (point)) 'invisible))
          (goto-char (next-single-char-property-change (point) 'invisible))
          (and (eolp) (beginning-of-line 2)))
        (setq eol (point)))
      (cond
       ((= eos eoh)
        'empty-node)
       ((or (>= eol eos)
            (not (string-match "\\S-" (buffer-substring eol eos))))
        'folded)
       (t
        'not-folded))))))

(defun my/org-tree-can-fold-p ()
  (not (member (my/org-get-folded-state) (list 'folded 'empty-node))))

(defun my/org-cycle-until-folded ()
  (while (my/org-tree-can-fold-p)
    (org-cycle)))

(defun my/org-hide-done-entries-in-range (start end)
  (save-excursion
    (goto-char end)
    (while (and (outline-previous-heading) (> (point) start))
      (when (org-entry-is-done-p)
        (my/org-cycle-until-folded)))))

(defun my/org-hide-done-entries-in-region (start end)
  (interactive "r")
  (my/org-hide-done-entries-in-range start end))

(defun my/org-hide-done-entries-in-buffer ()
  (interactive)
  (my/org-hide-done-entries-in-range (point-min) (point-max)))

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

;;;; GLOBAL KEYBOARD DEFINITIONS

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

(gsk #'consult-imenu "M-g m" "M-g M-m" "M-g f" "M-g M-f")

(gsk #'project-find-file "C-c C-p C-f" "C-c C-p f" "C-c p f")
(gsk #'project-switch-project "C-c C-p C-p" "C-c C-p p" "C-c p p")


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

;; default bindings (in isearch mode)
;; M-s h r   => highlight isearch results
;; M-s o      => open occur buffer with results

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

;; never ask about killing a ongoing build!
(setq compilation-always-kill t)

;; never ask about connected clients when quitting!
(remove-hook 'kill-emacs-query-functions #'server-kill-emacs-query-function)


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
  ;; enable imenu sections by ;;;;
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t)

  ;; many macros start with def-something. We want those usages in imenu.
  (add-to-list 'imenu-generic-expression
               '(nil "^\\s-*(def\\sw+\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 1 ) t)
  (add-to-list 'imenu-generic-expression
               '(nil "^\\s-*(use-package \\(\\(\\sw\\|\\s_\\)+\\)" 1 ) t)

  ;; many macros start with def-something. We want those usages in imenu.
  (add-to-list 'imenu-generic-expression
               '(nil "^\\s-*(ert-deftest\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 1 ) t)

  (lsk 'macrostep-expand "C-c C-e")
  (lsk 'eval-buffer "C-c C-c")

  (lsk #'my-ert-run-tests "<C-f5>")

  ;; we want documentation
  (eldoc-mode t))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'ielm-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; common lisp
(defhook lisp-mode-hook
         ;; we know this one from VS :)
         ;; we can also check symbols in slime with M-.
         (lsk 'slime "<f5>"))

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
         ;; for clojure comments - override annoying elisp mode single-; comment-indentation.
         (setq comment-column 0))

;; haskell
(defhook haskell-mode-hook
         ;; we want proper indentation
         (haskell-indent-mode +1))

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
             (define-key elpy-mode-map (kbd "C-c C-o") #'occur-dwim))))

;; lsp
(defhook lsp-mode-hook
         ;; lsp-mode does proper symbol highlghting natively.
         ;; disable generalized/regexp based symbol highlighting
         (highlight-symbol-mode 0)
         (eldoc-mode t)

         (lsk #'lsp-rename "C-c C-r")
         (lsk #'lsp-find-implementation "<f12>")
         (lsk #'lsp-find-definition "C-M-.")
         (lsk #'lsp-find-references "S-<f12>")

         (lsk #'lsp-execute-code-action "C-<return>" "C-M-<return>" "M-<return>"))

(defhook eglot-managed-mode-hook
         (require 'eglot)
         (when (eglot-managed-p)
           (lsk #'eglot-rename "C-c C-r")
           (lsk #'eglot-find-implementation "<f12>")
           (lsk #'eglot-find-declaration "C-M-.")
           (lsk #'xref-find-references "S-<f12>")
           (lsk #'eglot-code-actions "C-<return>" "C-M-<return>" "M-<return>")))
(defhook before-save-hook
         (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
           (eglot-format-buffer)))
(defhook magit-post-refresh-hook
         (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
           (eglot-reconnect)))


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

         ;; (my/org-hide-done-entries-in-buffer)
         )

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

         ;; navigate back again.
         ;; (could also use set-mark with prefix argument C-u C-spc.)
         (lsk 'pop-local-or-global-mark "C--")

         (lsk 'occur-dwim "C-c C-o")

         (hs-minor-mode 1)
         (local-unset-key (kbd "M-m"))
         (lsk 'hs-toggle-hiding "M-m M-m")


         ;; settings

         ;; highlight current function?
         (which-function-mode 1)

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

         ;; flyspell too!
         ;; (my-enable-flyspell-mode t)

         ;; we want teh completion
         (lsk #'completion-at-point "C-.")
         ;; use vertico/consult for completion
         (setq completion-in-region-function #'consult-completion-in-region)

         ;; inline code-completion
         (completion-preview-mode t)

         ;; try out eglot for a while
         (eglot-ensure)

         ;; highlight TODO-like comments, always
         (font-lock-add-keywords
          nil
          '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))

         ;; built in Emacs completion
         (setf completion-auto-select t ;; Show completion on first call
               completion-auto-help 'visible ;; Display *Completions* upon first request
               completions-format 'one-column ;; Use only one column
               completions-sort 'historical ;; Order based on minibuffer history
               completions-max-height 20 ;; Limit completions to 15 (completions start at line 5)
               completion-ignore-case t))

(add-hook 'toml-ts-mode-hook #'my-prog-mode-hook) ; TOML is programming-related!

;; xml
(defhook nxml-mode-hook
         (lsk 'comment-or-uncomment-region "C-c C-c")
         (lsk 'uncomment-region            "C-c C-u")

         (lsk 'nxml-where                  "C-c C-w")
         (lsk 'nxml-pretty-print-buffer    "C-c C-e")

         (lsk #'completion-at-point          "C-.")

         ;; causes entire elements (with children) to be treated as sexps.
         (setq nxml-sexp-element-flag t)

         ;; C-c C-o is bound to some useless heading/section things
         ;; we're not using. Restablish occur-dwim as a "global" binding.
         (local-unset-key (kbd "C-c C-o"))
         (local-unset-key (kbd "C-c"))
         (local-unset-key (kbd "C"))
         (local-set-key (kbd "C-c C-o") #'occur-dwim)

         ;; xml-files are more often than not part of a project.
         ;; (projectile-mode t)

         ;; autofill mode should NEVER be on for xml!
         (auto-fill-mode 0))

;; things like markdown
(defhook text-mode-hook
         ;; we do NOT want spell-checking in XML mode!
         (unless (is-mode-active-p 'nxml-mode)
           (my-enable-flyspell-mode nil))

         ;; make line-wraps where they should be according to
         ;; ancient conventions.
         (unless (is-mode-active-p 'yaml-ts-mode)
           (auto-fill-mode t))


         (lsk 'flyspell-correct-word-before-point "C-c C-k"))

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

  ;; Fix for issue which corrupts emacs' http client.
  ;; --
  ;; This will among other things caused emacs to break in a million ways
  ;; and packages never to download (and thus ruining the self-bootstrapping
  ;; process if the profile is 'corrupted':
  ;; --
  ;; https://emacs.stackexchange.com/questions/15020/eww-error-in-process-sentinel-url-cookie-generate-header-lines-wrong-type-arg/15153#15153"
  (delete-file
   (expand-file-name "~/.emacs.d/url/cookie"))
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
        (slime-setup '(slime-fancy slime-asdf slime-company))))))

(if (eq system-type 'windows-nt)
    (my-windows-mode-hook)
  (my-unix-mode-hook))


(defun my-macos-mode-hook ()
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil))

(if (eq system-type 'darwin)
    (my-macos-mode-hook))

;;;; FREEBSD ONLY CUSTOMIZATIONS


(defun my-freebsd-mode-hook ()
  ;; fix backsapce acting like delete, by making delete act like backspace.
  (normal-erase-is-backspace-mode 0))

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

(add-hook 'eww-after-render-hook 'my-set-eww-buffer-title)

(defun my-eww-reflow ()
  "Causes the current buffer to reflow if it's a eww-buffer."
  (interactive)
  (when (string= "eww-mode" major-mode)
    (eww-reload t nil)))

;; dirty-patch to compile.el
;; remove any incorrectly added entries!
;; (dolist (item compilation-error-regexp-alist)
;;   (let* ((value (alist-get item compilation-error-regexp-alist-alist)))
;;     (when (not value)
;;       (setq compilation-error-regexp-alist (remove item compilation-error-regexp-alist)))))


(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "screencapture" nil nil nil "-i" filename)
  (insert (concat "[[./" filename "]]"))
  (org-display-inline-images))

(defun my-org-paste-image ()
  "Paste an image into a time stamped unique-named file in the
  same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "pngpaste" nil nil nil filename)
  (insert (concat "[[./" filename "]]"))
  (org-display-inline-images))
