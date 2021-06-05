;; -*- coding: utf-8; lexical-binding: t; -*-
;;{{ All about version-control

;; NOTE: magit is good,
;; but it just got too complex,
;; Maybe I will remove this package later.
(my-add-package 'magit)
(with-eval-after-load 'magit
  (setq git-commit-style-convention-checks nil
		;; Supress message
		magit-no-message '("Turning on magit-auto-revert-mode...")
		magit-process-popup-time 30
		magit-ediff-dwim-show-on-hunks t
		magit-diff-refine-hunk t))

;; git-rebase
;; NOTE: This is part of magit.
(with-eval-after-load 'git-rebase
  (define-key git-rebase-mode-map "j" #'next-line)
  (define-key git-rebase-mode-map "k" #'previous-line))

;; git time machine
(my-add-package 'git-timemachine)
(setq git-timemachine-abbreviation-length 4)

;; Buildin version-control system, I love this!
(setq vc-follow-symlinks t
	  ;; auto revert git info(used by diff-hl)
	  vc-allow-async-revert t ; commit without all changes been found,just smoother
	  vc-make-backup-files nil ; Do not make backups of files, not safe
	  )

;; Highlight uncommitted changes using VC
(my-add-package 'diff-hl)
(add-hook 'after-init-hook 'global-diff-hl-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(with-eval-after-load 'diff-hl
  (setq diff-hl-draw-borders nil)
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  (diff-hl-margin-mode 1)

  ;; Integration with magit
  (with-eval-after-load 'magit
	(add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
	(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Change log
(after! add-log
  (setq add-log-keep-changes-together t))

;; {{ Diff two regions
;; Step 1: Select a region and `M-x diff-region-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-region-compare-with-b'
;; Press "q" in evil-mode or "C-c C-c" to exit the diff output buffer
;; from  https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-misc.el
(defun diff-region-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E"
  (let* (tmp rlt)
    ;; swap b e, make sure b < e
    (when (> b e)
      (setq tmp b)
      (setq b e)
      (set e tmp))

    ;; select lines
    (save-excursion
      ;; Another workaround for evil-visual-line bug:
      ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
      ;; the (line-beginning-position) of the line which is after the last selected
      ;; line is always (region-end)! Don't know why.
      (if (and (> e b)
               (save-excursion (goto-char e) (= e (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
          (setq e (1- e)))
      (goto-char b)
      (setq b (line-beginning-position))
      (goto-char e)
      (setq e (line-end-position)))
    (setq rlt (list b e))
    rlt))

(defmacro diff-region-open-diff-output (content buffer-name)
  `(let ((rlt-buf (get-buffer-create ,buffer-name)))
    (save-current-buffer
      (switch-to-buffer-other-window rlt-buf)
      (set-buffer rlt-buf)
      (erase-buffer)
      (insert ,content)
      ;; `diff-mode' is more powerful than `diff-mode'
      (diff-mode)
      (goto-char (point-min)))))

(defun diff-region-tag-selected-as-a ()
  "Select a region to compare."
  (interactive)
  (when (region-active-p)
    (let* (tmp buf)
      ;; select lines
      (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp))))
  (message "Now select other region to compare and run `diff-region-compare-with-b'"))

(defun diff-region-compare-with-b ()
  "Compare current region with the region set by `diff-region-tag-selected-as-a'.
If no region is selected, `kill-ring' or clipboard is used instead."
  (interactive)
  (let* (rlt-buf
         diff-output
         tmp
         ;; file A
         (fa (make-temp-file (expand-file-name "diff-region"
                                               (or small-temporary-file-directory
                                                   temporary-file-directory))))
         ;; file B
         (fb (make-temp-file (expand-file-name "diff-region"
                                               (or small-temporary-file-directory
                                                   temporary-file-directory)))))
    (when (and fa (file-exists-p fa) fb (file-exists-p fb))
      (cond
       ((region-active-p)
        ;; text from selected region
        (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
        (write-region (car tmp) (cadr tmp) fb))
       (t
        ;; text from `kill-ring' or clipboard
        (let* ((choice (completing-read "Since no region selected, compare text in:"
                                        '("kill-ring" "clipboard")))
               (txt (cond
                     ((string= choice "kill-ring")
                      (car kill-ring))
                     ((string= choice "clipboard")
                      (my-gclip)))))
          (with-temp-file fb
            (insert txt)))))
      ;; save region A as file A
      (save-current-buffer
        (set-buffer (get-buffer-create "*Diff-regionA*"))
        (write-region (point-min) (point-max) fa))
      ;; diff NOW!
      ;; show the diff output
      (cond
       ((string= (setq diff-output (shell-command-to-string (format "%s -Nabur %s %s" diff-command fa fb))) "")
        (message "Two regions are SAME!"))
       ((executable-find "git")
        (my-ensure 'magit)
        (magit-diff-setup nil (list "--no-index" "--indent-heuristic" "--histogram")
                          nil (list (magit-convert-filename-for-git
                                     (expand-file-name fa))
                                    (magit-convert-filename-for-git
                                     (expand-file-name fb))))
        (diff-mode))
       (t
        (diff-region-open-diff-output diff-output
                                      "*Diff-region-output*")))
      ;; clean the temporary files
      (if (and fa (file-exists-p fa))
          (delete-file fa))
      (if (and fb (file-exists-p fb))
          (delete-file fb)))))
;; }}

;; {{ Diff
(with-eval-after-load 'ediff
  (add-hook 'ediff-quit-hook 'winner-undo)
  (setq ediff-diff-options "-w" ;; turn off whitespace checking
		ediff-highlight-all-diffs t
		ediff-window-setup-function 'ediff-setup-windows-plain
		ediff-split-window-function 'split-window-horizontally
		ediff-merge-split-window-function 'split-window-horizontally)

  (defun ediff-copy-both-to-C (&optional arg)
	"Copy code from both A and B to C."
	(interactive)
	(ediff-copy-diff ediff-current-difference nil 'C nil
					 (concat
					  (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
					  (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)))))

;; minor mode for solve conflicts
(with-eval-after-load 'smerge-mode
  (defun my-transient-emerge ()
	"Use transient fo emerge actions."
	(interactive)
	(let ((echo-keystrokes nil)
		  (minibuffer-mes
		   "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
		 _l_ower              _>_: base/lower        _k_ill current
		 _a_ll                _R_efine
		 _RET_: current       _E_diff
"
		   ))
	  (message minibuffer-mes)
	  (set-transient-map
	   (let ((map (make-sparse-keymap)))
		 (define-key map (kbd "n") #'smerge-next)
		 (define-key map (kbd "p") #'smerge-prev)
		 (define-key map (kbd "b") #'smerge-keep-base)
		 (define-key map (kbd "u") #'smerge-keep-upper)
		 (define-key map (kbd "l") #'smerge-keep-lower)
		 (define-key map (kbd "a") #'smerge-keep-all)
		 (define-key map (kbd "RET") #'smerge-keep-current)
		 (define-key map (kbd "\C-m") #'smerge-keep-current)
		 (define-key map (kbd "<") #'smerge-diff-base-upper)
		 (define-key map (kbd "=") #'smerge-diff-upper-lower)
		 (define-key map (kbd ">") #'smerge-diff-base-lower)
		 (define-key map (kbd "R") #'smerge-refine)
		 (define-key map (kbd "E") #'smerge-ediff)
		 (define-key map (kbd "C") #'smerge-combine-with-next)
		 (define-key map (kbd "r") #'smerge-resolve)
		 (define-key map (kbd "k") #'smerge-kill-current)
		 (define-key map (kbd "q") #'quit)
		 map)
	   t)))
  
  )

(provide 'init-vc)
