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

;;; {{ This is little refactor from https://github.com/redguardtoo/counsel-etags
(defvar my-grep-keyword nil
  "Used in read keyword.")

(defvar my-grep-cands nil
  "Remember condidates of search for wgrep.")

(defvar my-grep-directory nil
  "Remember the directory for wgrep.")

(defun my-grep-unquote-regex-parens (str)
  "Unquote regexp parentheses in STR."
  (replace-regexp-in-string "\\\\[(){}]\\|[()]"
							(lambda (s)
							  (or (cdr (assoc s '(("\\(" . "(")
												  ("\\)" . ")")
												  ("(" . "\\(")
												  (")" . "\\)")
												  ("\\{" . "{")
												  ("\\}" . "}"))))
								  (error "Unexpected parenthesis: %S" s)))
							str t t))

(defun my-grep-read-keyword (hint)
  "Read keyword with HINT."
  (let* ((str (cond
			   ((region-active-p)
				(push (my-selected-str) grep-find-history)
				(my-selected-str))
			   (t
				(read-from-minibuffer hint
									  nil
									  nil
									  nil
									  'grep-find-history)))))
	(when str
	  (cond
	   ((region-active-p)
		(push str minibuffer-history)
		(setq my-grep-keyword (my-grep-unquote-regex-parens str))
		;; de-select region
		(set-mark-command nil))
	   (t
		;; processing double quotes character
		(setq my-grep-keyword (replace-regexp-in-string "\"" "\\\\\""str))))))
  my-grep-keyword)

(defun my-grep-forward-line (lnum)
  "Forward LNUM lines."
  (setq lnum (string-to-number lnum))
  (when (and lnum (> lnum 0))
	(goto-char (point-min))
	(forward-line (1- lnum))))

(defun my-grep-open-file-api (item dir &optional tagname)
  "Open ITEM while `default-directory' is DIR.
Focus on TAGNAME if it's not nil."
  ;; jump
  (let* ((is-str (and (stringp item)
					  (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'"
									item)))
		 (file (if is-str (match-string-no-properties 1 item)
				 (nth 0 item)))
		 (linenum (if is-str (match-string-no-properties 2 item)
					(nth 1 item)))
		 ;; always calculate path relative to TAGS
		 (default-directory dir))
	(cond
	 (t
	  ;; open file, go to certain line
	  (find-file file)
	  (my-grep-forward-line linenum)

	  ;; move focus to the tagname
	  (beginning-of-line)
	  ;; search tagname in current line might fail
	  ;; maybe tags files is updated yet
	  (when (and tagname
				 ;; focus on the tag if possible
				 (re-search-forward tagname (line-end-position) t))
		(goto-char (match-beginning 0)))

	  ;; flash, Emacs v25 only API
	  (xref-pulse-momentarily)))))

(defun my-grep-exclude-opts ()
  "Grep CLI options."
  ;; please note Windows DOS CLI only support double quotes
  (let ((ignore-dirs '("node_modules" ".vscode-server")) ; add it there,no extra variable for customize
		(ignore-file-names nil))
	(cond
	 ((executable-find "rg")
	  (concat (mapconcat (lambda (e)
						   (format "-g=\"!%s/*\"" (shell-quote-argument e)))
						 ignore-dirs " ")
			  " "
			  (mapconcat (lambda (e)
						   (format "-g=\"!%s\"" (shell-quote-argument e)))
						 ignore-file-names " ")))
	 (t
	  (concat (mapconcat (lambda (e)
						   (format "--exclude-dir=\"%s\"" (shell-quote-argument e)))
						 ignore-dirs " ")
			  " "
			  (mapconcat (lambda (e)
						   (format "--exclude=\"%s\"" (shell-quote-argument e)))
						 ignore-file-names " "))))))

(defun my-grep-cli (keyword)
  "Use KEYWORD and USE-CACHE to build CLI.
Extended regex is used, like (pattern1|pattern2)."
  (let ((my-grep-cli nil))
	(cond
	 ((executable-find "rg")
	  ;; "--hidden" force ripgrep to search hidden files/directories, that's default
	  ;; behavior of grep
	  (setq my-grep-cli (format "rg %s %s --hidden %s \"%s\" --"
								"-n -M 1024 --no-heading --color never -s --path-separator /"
								"" ; extra arguments
								(my-grep-exclude-opts)
								keyword)))
	 (t
	  ;; use extended regex always
	  (setq my-grep-cli (format "grep -rsnE %s %s -e \"%s\" *"
								"" ;; extra  arguments
								(my-grep-exclude-opts)
								keyword))))
	my-grep-cli))

(defun my-grep (&optional root)
  "Grep at project root directory or current directory.
Try to find best grep program (ripgrep, grep...) automatically.
Extended regex like (pattern1|pattern2) is used.
If DEFAULT-KEYWORD is not nil, it's used as grep keyword.
If HINT is not nil, it's used as grep hint.
ROOT is root directory to grep.
If SHOW-KEYWORD-P is t, show the keyword in the minibuffer."
  (interactive)
  (let* ((text (my-grep-read-keyword "Regular expression for grep: "))
		 (keyword text)
		 (default-directory (file-truename (or root
											   (cdr (project-current t)))))
		 (cmd (my-grep-cli keyword))
		 (cands (split-string (shell-command-to-string cmd) "[\r\n]+" t))
		 (dir-summary (my-fish-path default-directory 20))
		 (hint nil))
	(setq my-grep-cands cands)
	(setq my-grep-directory default-directory)
	(when (and cands
			   buffer-file-name
			   ;; string-distance is faster
			   (< (length cands) (* 4 256))
			   (fboundp 'string-distance))
	  ;; grep should not waste time on lisp version of string distance
	  ;; So `string-distance' from Emacs 27 is required
	  (let* ((ref (file-relative-name buffer-file-name root)))
		(setq cands
			  (sort cands
					`(lambda (a b)
					   (< (string-distance (car (split-string a ":")) ,ref t)
						  (string-distance (car (split-string b ":")) ,ref t)))))))

	;; Slow down grep 10 times
	(my-completing-read (concat hint (format "Grep \"%s\" at %s: "
											 text
											 dir-summary
											 ))
						cands
						:action `(lambda (item) ;; when grepping, we grepping in project root
								   (my-grep-open-file-api item
																   ,default-directory
																   ,keyword))
						:caller 'my-grep))
  ;; avoid too big data slows emacs
  (setq my-grep-cands nil))

(defun my-grep-at-current-directory ()
  "Grep at current directory."
  (interactive)
  (my-grep default-directory))

;; Could use wgrep easily without ivy
(defvar wgrep-header/footer-parser)
(defun my-grep-occur ()
  "Create a grep mode buffer listing LINES."
  (interactive)
  (let ((default-directory my-grep-directory)
		(grep-out-buffer  "*Export Grep*"))
	(let ((buf (get-buffer grep-out-buffer))
		  (cands my-grep-cands))
	  (unless buf
		(setq buf (generate-new-buffer grep-out-buffer)))
	  (with-current-buffer buf
		(erase-buffer) ; clean-buffer
		(insert (propertize "Exported grep results:\n\n" 'wgrep-header t))
		(dolist (line (mapcar
					   (lambda (cands) (concat "./" cands))
					   cands))
		  (insert line "\n"))
		;; avoid too big data slows emacs
		(setq my-grep-cands nil)
		(grep-mode)
		(wgrep-setup)
		(wgrep-change-to-wgrep-mode)
		(setq-local wgrep-header/footer-parser #'ignore))
	  (switch-to-buffer buf))))

;; powerful ivy-occur with wgrep
(after! ivy
  (defun my-grep-occur (&optional _cands)
	"Open occur buffer for `my-grep'."
	(unless (eq major-mode 'ivy-occur-grep-mode)
	  (ivy-occur-grep-mode)
	  (font-lock-mode -1))
	;; useless to set `default-directory', it's already correct
	;; we use regex in elisp, don't unquote regex
	(let* ((cands (ivy--filter ivy-text my-grep-cands)))
	  (swiper--occur-insert-lines
	   (mapcar
		(lambda (cand) (concat "./" cand))
		cands))))

  (ivy-set-occur 'my-grep 'my-grep-occur)
  (ivy-set-display-transformer 'my-grep 'counsel-git-grep-transformer))


;;; TODO: Do not store it with extra variable
(defun embark-target-completion-at-point (&optional relative)
  "Return the completion candidate at point in a completions buffer.
If the completions are file names and RELATIVE is non-nil, return
relative path."
    (if (not (get-text-property (point) 'mouse-face))
        (user-error "No completion here")
      ;; this fairly delicate logic is taken from `choose-completion'
      (let (beg end)
        (cond
         ((and (not (eobp)) (get-text-property (point) 'mouse-face))
          (setq end (point) beg (1+ (point))))
         ((and (not (bobp))
               (get-text-property (1- (point)) 'mouse-face))
          (setq end (1- (point)) beg (point)))
         (t (user-error "No completion here")))
        (setq beg (previous-single-property-change beg 'mouse-face))
        (setq end (or (next-single-property-change end 'mouse-face)
                      (point-max)))
        (let ((raw (buffer-substring-no-properties beg end)))
                  raw))))

(defun embark-completions-buffer-candidates ()
  "Return all candidates in a completions buffer."
  ;; when (derived-mode-p 'completion-list-mode)
     (save-excursion
       (goto-char (point-min))
       (next-completion 1)
       (let (all)
         (while (not (eobp))
           (push (embark-target-completion-at-point) all)
           (next-completion 1))
         (nreverse all))))

(defun sshit ()
  (interactive)
  (mes! (embark-completions-buffer-candidates)))


(defun consult-flymake--candidates ()
  "Return Flymake errors as alist."
  (let* ((raw-diags (or (flymake-diagnostics)
                        (user-error "No flymake errors (Status: %s)"
                                    (if (flymake-is-running) 'running 'finished))))
         (diags
          (mapcar
           (lambda (x)
             (let* ((buffer (flymake-diagnostic-buffer x))
                    (type (flymake-diagnostic-type x))
                    (type-str (propertize (format "%s"
                                                  (flymake--lookup-type-property
                                                   type 'flymake-type-name type))
                                          'face (flymake--lookup-type-property
                                                 type 'mode-line-face 'flymake-error)))
                    (narrow (pcase (flymake--lookup-type-property type 'flymake-category)
                              ('flymake-error ?e)
                              ('flymake-warning ?w)
                              (_ ?n))))
               (with-current-buffer buffer
                 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (flymake-diagnostic-beg x))
                     (list (buffer-name buffer)
                           (line-number-at-pos)
                           type-str
                           (flymake-diagnostic-text x)
                           (point-marker)
                           narrow))))))
           raw-diags))
         (buffer-width (apply #'max (mapcar (lambda (x) (length (nth 0 x))) diags)))
         (line-width (apply #'max (mapcar (lambda (x) (length (number-to-string (nth 1 x)))) diags)))
         (type-width (apply #'max (mapcar (lambda (x) (length (nth 2 x))) diags)))
         (fmt (format "%%-%ds %%-%dd %%-%ds %%s" buffer-width line-width type-width)))
    (mapcar
     (pcase-lambda (`(,buffer ,line ,type ,text ,marker ,narrow))
       (list (format fmt buffer line type text) marker narrow))
     (sort diags
           (pcase-lambda (`(_ _ ,t1 _ ,m1 _) `(_ _ ,t2 _ ,m2 _))
             (or (string< t1 t2) (and (string= t1 t2) (< m1 m2))))))))

(defun my-flymake-errors ()
  (interactive)
  (my-completing-read "Errors: "
					  (consult-flymake--candidates)
					  :action `(lambda (shit)
								 ;; (goto-line (cadr (assoc shit )))
								 (let ((char-pos (cadr shit)))
									   (goto-char char-pos)
									   (flymake-show-diagnostic char-pos)
									   ))))

(provide 'init-my-ffip)
;;; init-my-ffip.el ends here
