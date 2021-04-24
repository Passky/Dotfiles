;;{{ Dependency of 'init-xclip'
(defun my-buffer-str ()
  "Get string of current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun my-selected-str ()
  "Get string of selected region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun my-use-selected-string-or-ask (&optional hint)
  "Use selected region or ask for input.
If HINT is empty, use symbol at point."
  (cond
   ((region-active-p)
	(my-selected-str))
   ((or (not hint) (string= "" hint))
	(thing-at-point 'symbol))
   (t
	(read-string hint))))
;;}}

;;{{ autoload of xclip
(my-add-package 'xclip)
(autoload 'xclip-set-selection "xclip" "" t)
(autoload 'xclip-get-selection "xclip" "" t)

;; not use system clipboard
(setq select-enable-clipboard nil
	  ;; save current clipboard text
	  save-interprogram-paste-before-kill nil
	  ;; eliminate duplicates
	  kill-do-not-save-duplicates t)

(defun copy-yank-str (msg &optional clipboard-only)
  (unless clipboard-only (kill-new msg))
  (my-pclip msg)
  msg)

;; if in (display-graphic-p) state, use builtin robust solution
(defun my-evil-paste-from-+ ()
  "Paste from evil + register."
  (interactive)
  (let ((evil-this-register ?+))
	(call-interactively 'evil-paste-after)))

(defun my-evil-copy-to-+ ()
  "Copy from evil + register."
  (interactive)
  (let ((evil-this-register ?+))
	(call-interactively 'evil-yank)))
;;}}

(defun my-select-from-kill-ring (fn)
  "If N > 1, yank the Nth item in `kill-ring'.
If N is nil, use `ivy-mode' to browse `kill-ring'."
  (interactive "P")
  (let* ((candidates (cl-remove-if
					  (lambda (s)
						(or (< (length s) 5)
							(string-match-p "\\`[\n[:blank:]]+\\'" s)))
					  (delete-dups kill-ring)))
		 (ivy-height (/ (frame-height) 2)))
	(ivy-read "Browse `kill-ring':"
			  (mapcar #'my-prepare-candidate-fit-into-screen candidates)
			  :action fn)))

;; The windows part has compability with wsl
(defun my-gclip ()
  "Get clipboard content."
  (let* ((powershell-program (executable-find "powershell.exe")))
	(cond
	 ;; Windows
	 ((fboundp 'w32-get-clipboard-data)
	  ;; `w32-set-clipboard-data' makes `w32-get-clipboard-data' always return null
	  (w32-get-clipboard-data))

	 ;; Windows 10
	 (powershell-program
	  (string-trim-right
	   (with-output-to-string
		 (with-current-buffer standard-output
		   (call-process powershell-program nil t nil "-command" "Get-Clipboard")))))

	 ;; xclip can handle
	 (t
	  (xclip-get-selection 'clipboard)))))

(defun my-pclip (str-val)
  "Put STR-VAL into clipboard."
  (let* ((win64-clip-program (executable-find "clip.exe"))
		 ssh-client)
	(cond
	 ;; Windows
	 ((fboundp 'w32-set-clipboard-data)
	  (w32-set-clipboard-data str-val))

	 ;; Windows 10
	 ((and win64-clip-program)
	  (with-temp-buffer
		(insert str-val)
		(call-process-region (point-min) (point-max) win64-clip-program)))

	 ;; xclip can handle
	 (t
	  (xclip-set-selection 'clipboard str-val)))))
;;{{

(defun my-copy-fullpath-of-current-buffer ()
  "Copy full path into the yank ring and OS clipboard"
  (interactive)
  (when buffer-file-name
	(copy-yank-str (file-truename buffer-file-name))
	(message "file full path => clipboard & yank ring")))

(defun my-clipboard-to-kill-ring ()
  "Copy from clipboard to `kill-ring'."
  (interactive)
  (let* ((warning-minimum-level :emergency))
	(kill-new (my-gclip)))
  (message "clipboard => kill-ring"))

(defun my-kill-ring-to-clipboard ()
  "Copy from `kill-ring' to clipboard."
  (interactive)
  (my-select-from-kill-ring (lambda (s)
							  (let* ((summary (car s))
									 (hint " => clipboard" )
									 (msg (if (string-match-p "\.\.\.$" summary)
											  (substring summary 0 (- (length summary) (length hint)))
											msg)))
								;; cc actual string
								(my-pclip (cdr s))
								;; echo
								(message "%s%s" msg hint)))))

(defun my-paste-from-x-clipboard(&optional n)
  "Remove selected text and paste string clipboard.
If N is 1, we paste diff hunk whose leading char should be removed.
If N is 2, paste into `kill-ring' too.
If N is 3, converted dashed to camelcased then paste.
If N is 4, rectangle paste. "
  (interactive "P")
  (when (and (functionp 'evil-normal-state-p)
			 (functionp 'evil-move-cursor-back)
			 (evil-normal-state-p)
			 (not (eolp))
			 (not (eobp)))
	(forward-char))
  (let* ((str (my-gclip))
		 (fn 'insert))

	;; past a big string, stop lsp temporarily
	(when (and (> (length str) 1024)
			   (boundp 'lsp-mode)
			   lsp-mode)
	  (lsp-disconnect)
	  (run-at-time 5 nil  #'lsp-deferred)) ; stop lsp server temporarily to enhance performance

	(my-delete-selected-region)

	;; paste after the cursor in evil normal state
	(cond
	 ((not n)) ; do nothing
	 ((= 1 n)
	  (setq str (replace-regexp-in-string "^\\(+\\|-\\|@@ $\\)" "" str)))
	 ((= 2 n)
	  (kill-new str))
	 ((= 3 n)
	  (setq str (mapconcat (lambda (s) (capitalize s)) (split-string str "-") "")))
	 ((= 4 n)
	  (setq fn 'insert-rectangle)
	  (setq str (split-string str "[\r]?\n"))))
	(funcall fn str)))

(defun my-copy-to-x-clipboard (&optional num)
  "If NUM equals 1, copy the downcased string.
  If NUM equals 2, copy the captalized string.
  If NUM equals 3, copy the upcased string.
  If NUM equals 4, indent 4 spaces."
  (interactive "P")
  ;; see init-func
  (let* ((thing (my-use-selected-string-or-ask "")))
	(if (region-active-p) (deactivate-mark))
	(cond
	 ((not num))
	 ((= num 1)
	  (setq thing (downcase thing)))
	 ((= num 2)
	  (setq thing (capitalize thing)))
	 ((= num 3)
	  (setq thing (upcase thing)))
	 ((= num 4)
	  (setq thing (string-trim-right (concat "    "
											 (mapconcat 'identity (split-string thing "\n") "\n    ")))))
	 (t
	  (message "C-h f my-copy-to-x-clipboard to find right usage")))

	(my-pclip thing)
	(if (not (and num (= 4 num))) (message "kill-ring => clipboard")
	  (message "thing => clipboard!"))))


;; a warp for function
(defun my-system-paste ()
  "Paste from system clipboard."
  (interactive)
  (if (display-graphic-p)
	  (my-evil-paste-from-+)
	(my-paste-from-x-clipboard)))

(defun my-system-copy ()
  "Copy to system clipboard."
  (interactive)
  (if (display-graphic-p)
	  (my-evil-copy-to-+)
	(my-copy-to-x-clipboard)
	))

(provide 'init-clipboard)
;;; init-clipboard.el ends here.
