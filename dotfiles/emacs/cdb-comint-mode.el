;; cdb-comint-mode.el --- Emacs major-mode for inferior cdb/windbg process.

;; Author   : Jostein Kjønigsen <jostein@gmail.com>
;; Created  : October 2016
;; Version  : 0.0.1
;; Keywords : inferior-mode, convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; cdb-comint-mode.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; at your option any later version.

;; cdb-comint-mode.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:

;; cdb-comint-mode is a comint mode for Emacs which allows you to run a
;; cdb.exe (command line version of windbg) in a inferior process,
;;
;; Main features:
;; - Syntax highlighting.
;; - Automatic SOS loading, across .NET versions.
;; - Clickable pointers.

;; Usage:
;;  Put cdb-comint-mode.el in your load path
;;  Add (require 'cdb-comint-mode) to your .emacs or ~/.emacs.d/init.el
;;
;;  Do: `M-x cdb-dump'
;;  Away you go.

;;; Code:

(require 'comint)

(defvar cdb-custom-update-function nil
  "Function to call upon cdb-comint-mode buffer changes.")

(defvar cdb--pointer-update-pos nil)

(defun cdb--update-hightlight (&optional string)
  ;; highlight typical commands
  (hi-lock-face-buffer "\s!\\w+" 'hi-green-b)
  (hi-lock-face-buffer "\s\\.\\w+" 'hi-green-b)
  ;; monitors are always ugly
  (hi-lock-face-buffer "Monitor.Enter" 'hi-red-b)
  ;; exceptions
  (hi-lock-face-buffer "[A-Za-z]+Exception" 'hi-red-b)

  (cdb--update-pointers)

  (when cdb-custom-update-function
    (funcall cdb-custom-update-function)))

;; TODO: make configurable instead.
;; TODO: make option to launch 32-bit?
(defun cdb--get-exe-path ()
  "C:/Program Files (x86)/Windows Kits/8.1/Debuggers/x64/cdb.exe")

(defun cdb-run (&rest arguments)
  (interactive)
  (let ((buffer (get-buffer-create "*cdb*")))
    (switch-to-buffer buffer)
    (cdb-comint-mode)
    (setq-local comint-prompt-regexp "[0-9]+:[0-9]+>")
    (apply #'make-comint "cdb" (cdb--get-exe-path)
           nil arguments)))

(defun cdb-dump (filename)
  (interactive "fCrash dump file: ")
  (cdb-run "-z" filename))

;; actual major mode

(define-derived-mode cdb-comint-mode comint-mode "CDB"
  (make-local-variable 'comint-output-filter-function)
  (setq-local comint-output-filter-functions #'cdb--update-hightlight)
  (setq-local cdb--pointer-pos nil)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  (cdb--update-hightlight))

;; make pointers clicky
(defconst cdb--pointer-regexp "[a-fA-F0-9]\\{8,16\\}")

(defun cdb--update-pointers ()
  (interactive)
  (save-excursion
    ;; optimized update
    (if cdb--pointer-update-pos
        (goto-char cdb--pointer-update-pos)
      (beginning-of-buffer))

    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-1] #'cdb--dump-object)
      (define-key map [mouse-3] #'cdb--write-pointer)
      (while (re-search-forward cdb--pointer-regexp nil t)
        (let ((start (match-beginning 0))
              (end   (match-end 0)))
          (add-text-properties
           start end (list 'mouse-face 'highlight
                           'help-echo   "mouse-2: dump the object represented by this link"
                           'keymap      map)))))
    (setq cdb--pointer-update-pos (point-max))))

(defun cdb--pointer-at-point ()
  (interactive)
  (substring-no-properties (thing-at-point 'symbol)))

(defun cdb--write-pointer (event)
  (interactive "e")

  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event)))
        file)
    (if (not (windowp window))
        (error "No pointer found"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (let ((pointer (cdb--pointer-at-point)))
        (goto-char (point-max))
        (insert pointer)))))

(defun cdb--dump-object (event)
  (interactive "e")

  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event)))
        file)
    (if (not (windowp window))
        (error "No pointer found"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (let ((pointer (cdb--pointer-at-point)))
        (goto-char (point-max))
        (insert (concat "!do " pointer)))
      (comint-send-input))))


;; utility functions

(defun cdb-send-command (string)
  (let ((buffer (get-buffer-create "*cdb*")))
    (comint-send-string (get-buffer-process buffer)
                        (concat string "\n"))))

(defun cdb-load-sos ()
  (interactive)
  (cdb-send-command ".sympath %temp%\\symbolcache")
  (cdb-send-command ".symfix+")
  (cdb-send-command ".cordll -ve -u -l")
  (cdb-send-command ".reload"))

;; superoffice-specific
(defun my-cdb-custom-highlight-function ()
  ;; Some things should light up subtly in log
  (hi-lock-face-buffer "SuperOffice[A-Za-z0-9\s\\._\\+-]+" 'hi-cyan-b))
(setq cdb-custom-update-function 'my-cdb-custom-highlight-function)


(provide 'cdb-comint-mode)
;;; cdb-comint-mode.el ends here
