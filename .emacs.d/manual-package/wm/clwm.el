;;; wm.el --- Tiling windows manager in emacs. -*- lexical-binding: t -*-

;; Author: Per Vognsen <firstname.lastname@gmail.com>
;; Maintainer: passky <cmpassky@outlook.com>
;; Keywords: Tiling windows manager.
;; URL: https://gist.github.com/pervognsen/ee74944453cdc4b809cb

;;; Commentary:
;; `wm.el' is a Dynamic tiling window manager for Emacs (inspired by dwm/awesome/xmonad for Linux)
;; TODO: Why it conflicts with super-save?
;; TODO: Error processing message (wrong-type-argument window-live-p #<window num>).

;; Install:
;; manually by add wm.el to your `load-path', something like
;;
;;   (add-to-list 'load-path "path/to/wm.el")
;;
;; Setup:
;;
;; To use this package, add following code to your init.el or .emacs
;; (global-tlwm-mode)
;;
;; If you want to use the default keybinds,
;; add following code before require wm.el
;; (setq tlwm-default-bind t)

;;; code:

(require 'cl-lib)

(cl-defstruct tlwm-window buffer (point 0) (start 0) (hscroll 0) dedicated)

(defgroup tlwm nil
  "Windows manager."
  :group 'window
  :prefix 'tlwm-)

(defvar tlwm-windows nil)
(defvar tlwm-windows-alist nil)
(defvar tlwm-focus nil)
(defvar tlwm-workspace 0)
(defvar tlwm-workspaces nil)
(defvar tlwm-layout 0)
(defvar tlwm-ignored-buffers
  '("*LV*"  "*Calc Trail*"))

(defvar tlwm-layouts '(tlwm-layout-stacked-columns
					 tlwm-layout-stacked-rows
					 tlwm-layout-grid
					 tlwm-layout-bisection
					 tlwm-layout-fullscreen))

(defun tlwm-ignored-buffers-p ()
  (when (or (memq (buffer-name) tlwm-ignored-buffers))
	t))

(defun tlwm-window-from-emacs-window (window)
  (make-tlwm-window :buffer (window-buffer window)
				  :point (window-point window)
				  :start (window-start window)
				  :hscroll (window-hscroll window)
				  :dedicated (window-dedicated-p window)))

(defun tlwm-window-from-buffer (buffer)
  (make-tlwm-window :buffer buffer))

(defun tlwm-restore-window (window)
  (when tlwm-window-lived window ; TODO: why we still can't move window?
		(set-window-buffer nil (tlwm-window-buffer window))
		(set-window-point nil (tlwm-window-point window))
		(set-window-start nil (tlwm-window-start window))
		(set-window-hscroll nil (tlwm-window-hscroll window))
		(set-window-dedicated-p nil (tlwm-window-dedicated window))))

(defun tlwm-emacs-windows ()
  (window-list nil nil (frame-first-window)))

(defun tlwm-update-windows ()
  (dolist (window (tlwm-emacs-windows))
	(when-let (kv (assoc window tlwm-windows-alist))
	  (setf (nth (cdr kv) tlwm-windows) (tlwm-window-from-emacs-window window)))))

(defun tlwm-update-focus ()
  (when-let (kv (assoc (selected-window) tlwm-windows-alist))
	(setq tlwm-focus (cdr kv))))

(defun tlwm-status ()
  (let ((status ""))
	(dotimes (n (length tlwm-windows))
	  (let ((focused (= n tlwm-focus))
			(window (nth n tlwm-windows)))
		(setq status (format "%s%-20s"
							 status
							 (concat (if focused "[" " ")
									 (format "%d: %s" (1+ n) (buffer-name (tlwm-window-buffer window)))
									 (if focused "] " "  "))))))
	(format "%-35s %s" (format "<%d> %s: " (1+ tlwm-workspace) (symbol-name (nth tlwm-layout tlwm-layouts)))  status)))

(defun tlwm-display-status ()
  (let ((message-log-max nil)
		(message-truncate-lines t))
	(message (tlwm-status))))

(defun tlwm-reset-layout ()
  (delete-other-windows)
  (split-window)
  (other-window 1)
  (delete-other-windows))

(defun tlwm-layout-fullscreen ()
  (tlwm-reset-layout)
  (tlwm-restore-window (nth tlwm-focus tlwm-windows))
  (setq tlwm-windows-alist (list (cons (selected-window) tlwm-focus))))

(defun tlwm-layout-grid ()
  (tlwm-reset-layout)
  (let* ((n 0)
		 (len (length tlwm-windows))
		 (sqrt-len (truncate (sqrt len)))
		 (dim (if (= len (* sqrt-len sqrt-len)) sqrt-len (1+ sqrt-len))))
	(dotimes (y (1- (/ (+ len (1- dim)) dim)))
	  (split-window-vertically))
	(while (< n len)
	  (dotimes (x dim)
		(when (< n len)
		  (tlwm-restore-window (nth n tlwm-windows))
		  (cl-incf n))
		(when (and (< x (1- dim)) (< n len))
		  (split-window-horizontally)
		  (other-window 1)))
	  (other-window 1)))
  (balance-windows)
  (other-window tlwm-focus)
  (let ((n -1))
	(setq tlwm-windows-alist (mapcar (lambda (window) (cons window (cl-incf n)))
								   (tlwm-emacs-windows)))))

(defun tlwm-layout-bisection ()
  (tlwm-reset-layout)
  (dotimes (n (length tlwm-windows))
	(tlwm-restore-window (nth n tlwm-windows))
	(when (< n (1- (length tlwm-windows)))
	  (funcall (nth (mod n 2) '(split-window-horizontally split-window-vertically)))
	  (other-window 1)))
  (other-window (1+ tlwm-focus))
  (balance-windows)
  (let ((n -1))
	(setq tlwm-windows-alist (mapcar (lambda (window) (cons window (cl-incf n)))
								   (tlwm-emacs-windows)))))

(defun tlwm-layout-stacked-columns ()
  (tlwm-reset-layout)
  (tlwm-restore-window (cl-first tlwm-windows))
  (when (cl-rest tlwm-windows)
	(split-window-horizontally)
	(other-window 1)
	(cl-loop for (window . more-windows) on (cl-rest tlwm-windows)
			 do (progn
				  (tlwm-restore-window window)
				  (when more-windows
					(split-window-vertically)
					(other-window 1))))
	(balance-windows)
	(other-window (1+ tlwm-focus)))
  (let ((n -1))
	(setq tlwm-windows-alist (mapcar (lambda (window) (cons window (cl-incf n)))
								   (tlwm-emacs-windows)))))

(defun tlwm-layout-stacked-rows ()
  (tlwm-reset-layout)
  (tlwm-restore-window (cl-first tlwm-windows))
  (when (cl-rest tlwm-windows)
	(split-window-vertically)
	(other-window 1)
	(cl-loop for (window . more-windows) on (cl-rest tlwm-windows)
			 do (progn
				  (tlwm-restore-window window)
				  (when more-windows
					(split-window-horizontally)
					(other-window 1))))
	(balance-windows)
	(other-window (1+ tlwm-focus)))
  (let ((n -1))
	(setq tlwm-windows-alist (mapcar (lambda (window) (cons window (cl-incf n)))
								   (tlwm-emacs-windows)))))

(defun tlwm-update-layout ()
  (tlwm-update-windows)
  (funcall (nth tlwm-layout tlwm-layouts))
  (tlwm-display-status))

(defun tlwm-cycle-layout ()
  (interactive)
  (when (= (cl-incf tlwm-layout) (length tlwm-layouts))
	(setq tlwm-layout 0))
  (tlwm-update-focus)
  (tlwm-update-windows)
  (tlwm-update-layout))

(defun tlwm-focus-next-window ()
  (interactive)
  (tlwm-update-windows)
  (tlwm-update-focus)
  (if (assoc (selected-window) tlwm-windows-alist)
	  (when (= (cl-incf tlwm-focus) (length tlwm-windows))
		(setq tlwm-focus 0))
	(other-window 1)
	(tlwm-update-focus))
  (tlwm-update-layout))

(defun tlwm-focus-previous-window ()
  (interactive)
  (tlwm-update-windows)
  (tlwm-update-focus)
  (if (assoc (selected-window) tlwm-windows-alist)
	  (when (= (cl-decf tlwm-focus) -1)
		(setq tlwm-focus (1- (length tlwm-windows))))
	(other-window -1)
	(tlwm-update-focus))
  (tlwm-update-layout))

(defun tlwm-remove-nth (n lst)
  (when lst
	(if (zerop n)
		(cl-rest lst)
	  (cons (cl-first lst) (tlwm-remove-nth (1- n) (cl-rest lst))))))

(defun tlwm-delete (n)
  (when (and (> (length tlwm-windows) 1) (< n (length tlwm-windows)))
	(setq tlwm-windows (tlwm-remove-nth n tlwm-windows))
	(setq tlwm-windows-alist nil)
	(when (= tlwm-focus (length tlwm-windows))
	  (cl-decf tlwm-focus)))
  (tlwm-update-layout))

(defun tlwm-insert-nth (lst n val)
  (if (= n 0)
	  (cons val lst)
	(cons (cl-first lst) (tlwm-insert-nth (cl-rest lst) (1- n) val))))

(defun tlwm-insert (window n)
  (tlwm-update-focus)
  (tlwm-update-windows)
  (setq tlwm-windows (tlwm-insert-nth tlwm-windows n window))
  (setq tlwm-windows-alist nil)
  (when (>= tlwm-focus n)
	(cl-incf tlwm-focus))
  (tlwm-update-layout))

(defun tlwm-push-window ()
  (interactive)
  (tlwm-update-focus)
  (tlwm-update-windows)
  (setq tlwm-windows (append tlwm-windows (list (tlwm-window-from-emacs-window (selected-window)))))
  (setq tlwm-windows-alist nil)
  (tlwm-update-layout))

(defun tlwm-insert-window ()
  (interactive)
  (tlwm-update-focus)
  (tlwm-update-windows)
  (tlwm-insert (tlwm-window-from-emacs-window (selected-window)) (1+ tlwm-focus)))

(defun tlwm-pop-window ()
  (interactive)
  (tlwm-delete (1- (length tlwm-windows))))

(defun tlwm-delete-next-window ()
  (interactive)
  (tlwm-update-focus)
  (if (assoc (next-window) tlwm-windows-alist)
	  (tlwm-delete (mod (1+ tlwm-focus) (length tlwm-windows)))
	(delete-window (next-window))))

(defun tlwm-delete-window ()
  (interactive)
  (tlwm-update-focus)
  (if (assoc (selected-window) tlwm-windows-alist)
	  (tlwm-delete tlwm-focus)
	(delete-window)))

(defun tlwm-move-window-forward ()
  (interactive)
  (tlwm-update-windows)
  (tlwm-update-focus)
  (when (< (1+ tlwm-focus) (length tlwm-windows))
	(cl-rotatef (nth tlwm-focus tlwm-windows) (nth (1+ tlwm-focus) tlwm-windows))
	(setq tlwm-windows-alist nil)
	(setq tlwm-focus (1+ tlwm-focus)))
  (tlwm-update-layout))

(defun tlwm-move-window-backward ()
  (interactive)
  (tlwm-update-windows)
  (tlwm-update-focus)
  (when (> tlwm-focus 0)
	(cl-rotatef (nth tlwm-focus tlwm-windows) (nth (1- tlwm-focus) tlwm-windows))
	(setq tlwm-windows-alist nil)
	(setq tlwm-focus (1- tlwm-focus)))
  (tlwm-update-layout))

(defun tlwm-move-window-to-back ()
  (interactive)
  (tlwm-update-focus)
  (tlwm-update-windows)
  (tlwm-push-window)
  (tlwm-delete-window)
  (setq tlwm-focus (1- (length tlwm-windows)))
  (tlwm-update-layout))

(defun tlwm-move-window-to-front ()
  (interactive)
  (tlwm-update-windows)
  (tlwm-insert (tlwm-window-from-emacs-window (selected-window)) 0)
  (tlwm-delete-window)
  (setq tlwm-focus 0)
  (tlwm-update-layout))

(defun tlwm-focus-window (n)
  (interactive "p")
  (when (< n (length tlwm-windows))
	(setq tlwm-focus n))
  (tlwm-update-layout))

(defun tlwm-manage-windows ()
  (interactive)
  (setq tlwm-focus (cl-position (selected-window) (tlwm-emacs-windows)))
  (setq tlwm-windows (mapcar 'tlwm-window-from-emacs-window (tlwm-emacs-windows)))
  (setq tlwm-windows-alist nil)
  (tlwm-update-layout))

(defun tlwm-restore-workspace (layout focus windows)
  (setq tlwm-layout layout)
  (setq tlwm-focus focus)
  (setq tlwm-windows windows)
  (setq tlwm-windows-alist nil)
  (tlwm-update-layout))

(defun tlwm-save-workspace (workspace)
  (tlwm-update-focus)
  (tlwm-update-windows)
  (unless (assoc workspace tlwm-workspaces)
	(push (cons workspace nil) tlwm-workspaces))
  (setf (cdr (assoc workspace tlwm-workspaces)) (list tlwm-layout tlwm-focus tlwm-windows)))


(defun tlwm-switch-workspace (workspace)
  (tlwm-save-workspace tlwm-workspace)
  (unless (assoc workspace tlwm-workspaces)
	(push (cons workspace (list tlwm-layout 0 (list (tlwm-window-from-emacs-window (selected-window))))) tlwm-workspaces))
  (let ((state (cdr (assoc workspace tlwm-workspaces))))
	(setq tlwm-workspace workspace)
	(tlwm-restore-workspace (cl-first state) (cl-second state) (cl-third state))))

(defun tlwm-state-update ()
  "Update wm state."
  (tlwm-update-windows)
  (tlwm-update-layout)
  (tlwm-update-focus))

(defun tlwm-global-enable ()
  "`window-state-change-functions'"
  (add-hook 'window-state-change-functions 'tlwm-state-update))

(defun tlwm-global-disable ()
  (remove-hook 'window-state-change-functions 'tlwm-state-update))

(defvar tlwm-keymap
  (let ((keymap (make-sparse-keymap)))
	(when (bound-and-true-p tlwm-default-bind)
	  (define-key keymap (kbd "M-RET") 'tlwm-cycle-layout)
	  (define-key keymap (kbd "M-=") 'tlwm-push-window)
	  (define-key keymap (kbd "M-+") 'tlwm-insert-window)
	  (define-key keymap (kbd "M--") 'tlwm-pop-window)
	  (define-key keymap (kbd "M-_") 'tlwm-delete-next-window)
	  (define-key keymap (kbd "M-0") 'tlwm-delete-window)
	  (define-key keymap (kbd "M-]") 'tlwm-move-window-backward)
	  (define-key keymap (kbd "M-\\") 'tlwm-move-window-forward)
	  (define-key keymap (kbd "M-}") 'tlwm-move-window-to-front)
	  (define-key keymap (kbd "M-|") 'tlwm-move-window-to-back)
	  (define-key keymap (kbd "C-<tab>") 'tlwm-focus-next-window)
	  (define-key keymap (kbd "C-S-<tab>") 'tlwm-focus-previous-window)
	  (define-key keymap (kbd "M-1") (lambda () (interactive) (tlwm-focus-window 0)))
	  (define-key keymap (kbd "M-2") (lambda () (interactive) (tlwm-focus-window 1)))
	  (define-key keymap (kbd "M-3") (lambda () (interactive) (tlwm-focus-window 2)))
	  (define-key keymap (kbd "M-4") (lambda () (interactive) (tlwm-focus-window 3)))
	  (define-key keymap (kbd "M-5") (lambda () (interactive) (tlwm-focus-window 4)))
	  (define-key keymap (kbd "M-6") (lambda () (interactive) (tlwm-focus-window 5)))
	  (define-key keymap (kbd "M-7") (lambda () (interactive) (tlwm-focus-window 6)))
	  (define-key keymap (kbd "M-8") (lambda () (interactive) (tlwm-focus-window 7)))
	  (define-key keymap (kbd "M-9") (lambda () (interactive) (tlwm-focus-window 8)))
	  (define-key keymap (kbd "M-<f1>") (lambda () (interactive) (tlwm-switch-workspace 0)))
	  (define-key keymap (kbd "M-<f2>") (lambda () (interactive) (tlwm-switch-workspace 1)))
	  (define-key keymap (kbd "M-<f3>") (lambda () (interactive) (tlwm-switch-workspace 2)))
	  keymap))
  "Keymap for wm.")

(define-minor-mode tlwm-mode
  "Tlwm-mode ."
  :init-value t
  :global t
  :keymap tlwm-keymap
  (tlwm-manage-windows))

(define-global-minor-mode global-tlwm-mode tlwm-mode
  (lambda ()
	(unless (or (tlwm-ignored-buffers-p) (minibufferp)) ; could be annoying with minibuffer
	  (tlwm-mode 1)))
  :group 'tlwm
  (if tlwm-mode
	  (tlwm-global-enable)
	(tlwm-global-disable))
  )

(provide 'wm)
;;; wm.el ends here
