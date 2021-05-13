;; This contains windows-nt system specific configration
;;; Code
;; PATH in windows
(when (file-exists-p "C:/Program Files/Git/usr/bin")
  (setenv "PATH"
          (concat
           "C:\\Program Files\\Git\\usr\\bin" ";"
           (getenv "PATH")))
  nil)

(when (file-exists-p "C:/Program Files (x86)/Git/usr/bin")
  (setenv "PATH"
          (concat
           "C:/Program Files (x86)/Git/usr/bin" ";"
           (getenv "PATH")))
  nil)

(when (file-exists-p "~/.local/bin")
  (setenv "PATH"
          (concat
           "~/.local/bin" ";"
           (getenv "PATH")))
  nil)


(setq w32-ignore-modifiers-on-IME-input nil
	  ;; do not ignore ctrl/alt/shift when input method is on
	  )

(when (functionp 'w32-set-ime-open-status)
  (defun emacs-ime-disable ()
	(w32-set-ime-open-status nil))

  (defun emacs-ime-enable ()
	(w32-set-ime-open-status t))

  (add-hook 'evil-insert-state-entry-hook #'emacs-ime-enable)
  (add-hook 'evil-motion-state-entry-hook #'emacs-ime-disable)
  (add-hook 'evil-normal-state-entry-hook  #'emacs-ime-disable)
  (add-hook 'evil-replace-state-entry-hook #'emacs-ime-disable)
  (add-hook 'evil-visual-state-entry-hook #'emacs-ime-disable)
  (add-hook 'evil-emacs-state-entry-hook #'emacs-ime-disable)
  (add-hook 'org-capture-after-finalize-hook #'emacs-ime-disable))

(provide 'init-windows)
;;; init-windows.el ends here.
