;;; Includes most useful function for other function
(autoload 'ivy-read "ivy")

;; https://github.com/mgalgs/.emacs.d/blob/8eace8e06ac441c3092a2fbdceda3bc2ec985cf3/lisp/my-util.el
;; `lsp-java--completing-read'
(cl-defun my-completing-read (prompt collection &key action hist require-match initial-input)
  "Wrap for `completing-read' and `ivy-read'.
  Find detailed information in `completing-read'."
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
	  ;; (setq selected (or (assoc selected collection) selected))
	  ;; make sure only the string/file is passed to action
	  (if action (mapc action selected))
	  selected))
   ))

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

(defmacro bind-key (key-name command &optional keymap predicate)
  "Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).
KEY-NAME may be a vector, in which case it is passed straight to
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.
COMMAND must be an interactive function or lambda form.
KEYMAP, if present, should be a keymap variable or symbol.
For example:
  (bind-key \"M-h\" #'some-interactive-function my-mode-map)
  (bind-key \"M-h\" #'some-interactive-function 'my-mode-map)
If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time."
  (let ((namevar (make-symbol "name"))
		(keyvar (make-symbol "key"))
		(kdescvar (make-symbol "kdesc"))
		(bindingvar (make-symbol "binding")))
	`(let* ((,namevar ,key-name)
			(,keyvar (if (vectorp ,namevar) ,namevar
					   (read-kbd-macro ,namevar)))
			(kmap (if (and ,keymap (symbolp ,keymap)) (symbol-value ,keymap) ,keymap))
			(,kdescvar (cons (if (stringp ,namevar) ,namevar
							   (key-description ,namevar))
							 (if (symbolp ,keymap) ,keymap (quote ,keymap))))
			(,bindingvar (lookup-key (or kmap global-map) ,keyvar)))
	   ,(if predicate
			`(define-key (or kmap global-map) ,keyvar
			   '(menu-item "" nil :filter (lambda (&optional _)
											(when ,predicate
											  ,command))))
		  `(define-key (or kmap global-map) ,keyvar ,command)))))

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


(provide 'init-misc)
;;; init-misc.el ends here
