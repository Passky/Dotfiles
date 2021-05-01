;;; Includes most useful function for other function

;; (setq selected (or (assoc selected collection) selected))
;; make sure only the string/file is passed to action
(cl-defun my-completing-read (prompt collection &key action hist require-match initial-input)
  "Wrap for `completing-read-multiple',
split cands with `crm-separator'(default ,)."
  (cond
   ((and action (= 0 (length collection)))
	(message "No candidates."))
   ;; compatable with other completion system.
   (t
	(let* ((selected (completing-read-multiple
					  prompt
					  collection
					  nil ; predicate
					  require-match
					  initial-input
					  hist)))
	  (mapc action selected)
	  selected))))

(defvar my-project-file '(".svn" ".hg" ".git" ".root" "makefile" "Makefile")
  "The file/directory used to locate project root.
May be set using .dir-locals.el.  Checks each entry if set to a list.")

;; NOTE: or just use
;; (cdr (project-current))
(defun my-project-root ()
  "Return project root or `default-directory'."
  (let* ((project-root  (cl-some (apply-partially 'locate-dominating-file
												  default-directory)
								 my-project-file)))
	(or (and project-root (file-name-as-directory project-root))
		default-directory)))

(defun my-pdb ()
  (interactive)
  (let* ((default-directory (cdr (project-current)))
		 (current-py-file (format "python -m pdb %s" buffer-file-name))
		 (command-to-execute (format "(pdb \"%s\")" current-py-file)))
	;; (command-execute command-to-execute)
	;; (mes! (commandp command-to-execute))
	))

(defun my-vsplit-window (&optional split-left?)
  (interactive)
  (split-window (selected-window)
				(if split-left? 'left 'right))
  (balance-windows (window-parent)))

(defun my-split-window (&optional split-above?)
  (interactive)
  (split-window (selected-window)
				(if split-above? 'below 'above))
  (balance-windows (window-parent))  )

;; for basic window move,there is
;; `windmove-up' and so on.
(defun my-prev-wind ()
  "Go to previous window."
  (catch 'done
	(dolist (buf (buffer-list (selected-frame)))
	  (let ((win (get-buffer-window buf)))
		(when (and (not (eq buf (current-buffer)))
				   win
				   (not (eq win (selected-window))))
		  (select-window win)
		  (throw 'done nil))))))

(defun my-buffer-new (&optional file)
  "Creates a new buffer replacing the current window,
optionally editing a certain FILE."
  (interactive "s")
  (if file
	  (find-file file)
	(let ((buffer (generate-new-buffer "*new*")))
	  (set-window-buffer nil buffer)
	  (with-current-buffer buffer
		(funcall (default-value 'major-mode))))))

(defun my-window-delete (&optional unbalance?)
  "Deletes the current window.
unless UNBALANCE?,
the deleted window's parent window are rebalanced."
  (let ((p (window-parent)))
	(delete-window)
	(unless unbalance?
	  ;; balance-windows raises an error if the parent does not have
	  ;; any further children (then rebalancing is not necessary anyway)
	  (condition-case nil
		  (balance-windows p)
		(error)))))

(defun my-resize-window (new-size &optional horizontal)
  "Set the current window's width or height to NEW-SIZE.
If HORIZONTAL is non-nil the width of the window is changed,
otherwise its height is changed."
  (let ((count (- new-size (if horizontal (window-width) (window-height)))))
	(enlarge-window count horizontal)))

(defun my-window-increase-height (count)
  "Increase current window height by COUNT."
  (interactive "p")
  (my-resize-window (+ (window-height) count)))

(defun my-window-decrease-height (count)
  "Decrease current window height by COUNT."
  (interactive "p")
  (my-resize-window (- (window-height) count)))

(defun my-window-increase-width (count)
  "Increase current window width by COUNT."
  (interactive "p")
  (my-resize-window (+ (window-width) count) t))

(defun my-window-decrease-width (count)
  "Decrease current window width by COUNT."
  (interactive "p")
  (my-resize-window (- (window-width) count) t))

(defun my-window-set-height (count)
  "Sets the height of the current window to COUNT."
  (interactive "<c>")
  (my-resize-window (or count (frame-height)) nil))

(defun my-window-set-width (count)
  "Set the width of the current window to COUNT."
  (interactive "<c>")
  (my-resize-window (or count (frame-width)) t))

(defun my-try-recentfile()
  (interactive)
  (let* ((selected (completing-read-multiple
					"Recentfiles: "
					recentf-list
					))
		 )
	;; make sure only the string/file is passed to action
	;; (funcall #'find-file (if (consp selected) (cdr selected) selected))
	(mes! selected)
	selected)
  )

(defun my-find-file-in-current-directory ()
  "A warp of `project-find-file'.
Use `project--files-in-directory'to
Find file at current directory."
  (interactive)
  ;; use transient project,calls `find-program'
  (let* ((pr (cons (or (car (project-current)) 'transient) default-directory))
		 (dirs (list (project-root pr))))
	(project-find-file-in (thing-at-point 'filename) dirs pr)
	))

(defun my-grep-at-current-directory-by-pr (regexp)
  "A wrap of `project-find-regexp'.
Grep at current directory by REGEXP."
  (interactive (list (project--read-regexp)))
  (require 'xref)
  (let* ((pr (cons (or (car (project-current)) 'transient) default-directory))
		 (dirs (list (project-root pr)))
		 (files
		  (project-files pr dirs)))
	(xref--show-xrefs
	 (apply-partially #'project--find-regexp-in-files regexp files)
	 nil)))

(provide 'init-misc)
;;; init-misc.el ends here
