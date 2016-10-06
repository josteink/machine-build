

(require 'comint)

(defvar cdb-custom-update-function nil
  "Function to call upon cdb-comint-mode buffer changes.")

(defvar cdb--pointer-update-pos nil)

(defun cdb--update-hightlight (&optional string)
  ;; highlight prompts
  (hi-lock-face-buffer "[0-9]+:[0-9]+>" 'hi-yellow)

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


(defun cdb--get-exe-path ()
  "C:/Program Files (x86)/Windows Kits/8.1/Debuggers/x64/cdb.exe")

(defun cdb-run (&rest arguments)
  (interactive)
  (let ((buffer (get-buffer-create "*cdb*")))
    (switch-to-buffer buffer)
    (cdb-comint-mode)
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
    (setq cdb--pointer-update-pos (end-of-buffer))))

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


;; superoffice-specific
(defun my-cdb-custom-highlight-function ()
  ;; Some things should light up subtly in log
  (hi-lock-face-buffer "SuperOffice[A-Za-z0-9\s\\._\\+-]+" 'hi-cyan-b))
(setq cdb-custom-update-function 'my-cdb-custom-highlight-function)
