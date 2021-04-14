;; copy lot from github.com/redguardtoo/emacs.d
;;{{ autoload
(my-add-package 'buffer-move)
(autoload 'buf-move-left "buffer-move" "move buffer" t)
(autoload 'buf-move-right "buffer-move" "move buffer" t)
(autoload 'buf-move-up "buffer-move" "move buffer" t)
(autoload 'buf-move-down "buffer-move" "move buffer" t)
(autoload 'dumb-jump-go "dumb-jump" "" t)
;;}}

(defun mes! (symbol)
  "Just tell me the value of SYMBOL."
  (message "%s" symbol))

;;{{ from doom
;; https://github.com/hlissner/doom-emacs
(defun file! ()
  "Return the emacs lisp file this macro is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
        (load-file-name)
        ((stringp (car-safe current-load-list))
         (car current-load-list))
        (buffer-file-name)
        ((error "Cannot get this file-path"))))

(defmacro letf! (bindings &rest body)
  "Temporarily rebind function and macros in BODY.

BINDINGS is either a) a list of, or a single, `defun' or `defmacro'-ish form, or
b) a list of (PLACE VALUE) bindings as `cl-letf*' would accept.

TYPE is either `defun' or `defmacro'. NAME is the name of the function. If an
original definition for NAME exists, it can be accessed as a lexical variable by
the same name, for use with `funcall' or `apply'. ARGLIST and BODY are as in
`defun'.

\(fn ((TYPE NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defmacro))
	(setq bindings (list bindings)))
  (dolist (binding (reverse bindings) (macroexpand body))
	(let ((type (car binding))
		  (rest (cdr binding)))
	  (setq
	   body (pcase type
			  (`defmacro `(cl-macrolet ((,@rest)) ,body))
			  (`defun `(cl-letf* ((,(car rest) (symbol-function #',(car rest)))
								  ((symbol-function #',(car rest))
								   (lambda ,(cadr rest) ,@(cddr rest))))
						 (ignore ,(car rest))
						 ,body))
			  (_
			   (when (eq (car-safe type) 'function)
				 (setq type (list 'symbol-function type)))
			   (list 'cl-letf (list (cons type rest)) body)))))))

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load', `write-region' and anything that
writes to `standard-output'."
  `(if doom-debug-p
       (progn ,@forms)
     ,(if doom-interactive-p
          `(let ((inhibit-message t)
                 (save-silently t))
             (prog1 ,@forms (message "")))
        `(letf! ((standard-output (lambda (&rest _)))
                 (defun message (&rest _))
                 (defun load (file &optional noerror nomessage nosuffix must-suffix)
                   (funcall load file noerror t nosuffix must-suffix))
                 (defun write-region (start end filename &optional append visit lockname mustbenew)
                   (unless visit (setq visit 'no-message))
                   (funcall write-region start end filename append visit lockname mustbenew)))
           ,@forms))))

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.

This is a wrapper around `eval-after-load' that:

1. Suppresses warnings for disabled packages at compile-time
2. Supports compound package statements (see below)
3. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent defun) (debug t))
  (if (symbolp package)
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              (let ((body (macroexp-progn body)))
                `(if (featurep ',package)
                     ,body
                   ;; We intentionally avoid `with-eval-after-load' to prevent
                   ;; eager macro expansion from pulling (or failing to pull) in
                   ;; autoloaded macros/packages.
                   (eval-after-load ',package ',body))))
    (let ((p (car package)))
      (cond ((not (keywordp p))
             `(after! (:and ,@package) ,@body))
            ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (cdr package))
               (setq body `((after! ,next ,@body))))
             (car body))))))

;;; Closure factories
(defmacro fn! (arglist &rest body)
  "Expands to (cl-function (lambda ARGLIST BODY...))"
  (declare (indent defun) (doc-string 1) (pure t) (side-effect-free t))
  `(cl-function (lambda ,arglist ,@body)))

(defmacro cmd! (&rest body)
  "Expands to (lambda () (interactive) ,@body).
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro cmd!! (command &optional prefix-arg &rest args)
  "Expands to a closure that interactively calls COMMAND with ARGS.
A factory for quickly producing interactive, prefixed commands for keybinds or
aliases."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,prefix-arg arg)))
       (,(if args
             'funcall-interactively
           'call-interactively)
        ,command ,@args))))

(defmacro cmds! (&rest branches)
  "Expands to a `menu-item' dispatcher for keybinds."
  (declare (doc-string 1))
  (let ((docstring (if (stringp (car branches)) (pop branches) ""))
        fallback)
    (when (cl-oddp (length branches))
      (setq fallback (car (last branches))
            branches (butlast branches)))
    `(general-predicate-dispatch ,fallback
       :docstring ,docstring
       ,@branches)))
;;}}

(defun my-delay-eval (my-func &optional delay)
  "Delay exec `my-func' after init, default `delay' is `0.7'"
  (let ((delay (or 0.7 delay)))
	(run-with-idle-timer delay nil my-func)))

(defmacro my-delay-after-mode (op-mode my-func &optional delay)
  "A delay exec MY-FUNC after OP-MODE, default DELAY is 1."
  (let ((delay (or delay 0.7))
		(funsymbol (intern (format "%s-mode-hook" op-mode) )))
	`(add-hook ',funsymbol '(lambda ()
							  (run-with-idle-timer ,delay nil ,my-func)
							  ))))

(defun my-ensure (feature)
  "Make sure `FEATURE' is required."
  (unless (featurep feature)
	(condition-case nil
		(require feature)
	  (error nil))))

;; update package in site-lisp-dir using magit submodule
(defun my-update-site-lisp-dir-git-submodule ()
  "Update package in site-lisp-dir using git shell command."
  (interactive)
  (let ((default-directory my-site-lisp-dir)
		(current-prefix-arg t))
	(my-async-shell-command (format "cd %s && git submodule foreach git pull" my-site-lisp-dir))))

(defun my-update-site-lisp-dir-git-submodule-magit ()
  "Update package in site-lisp-dir using magit submodule."
  (interactive)
  (let ((default-directory my-site-lisp-dir)
		(current-prefix-arg t))
	(my-ensure 'magit)
	(magit-submodule-update (magit-module-confirm "Update" 'magit-module-worktree-p) '("--remote"))))

;; {{ Dependency of Evil
(defun scroll-other-window-up ()
  (interactive)
  (scroll-other-window '-))

(defun my-force-quit-without-asking()
  "Quit emacs without asking."
  (interactive)
  (my-ensure 'recentf)
  (recentf-mode)
  (toggle-save-place-globally)
  (recentf-save-list)
  (savehist-save)
  (let ((proc (frame-parameter (selected-frame) 'client)))
	(if proc
		(with-no-warnings
		  (server-delete-client proc))
	  (dolist (process (process-list))
		(set-process-query-on-exit-flag process nil))
	  (kill-emacs))))

(defun my-sudo-save ()
  "Use tramp to save as sudoer."
  (interactive)
  (if (not buffer-file-name)
	  (write-file (concat "/sudo:root@localhost:" (read-file-name "Filename to save: ")))
	(write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun my-reload-emacs()
  "Reload application."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; from evil
(defun my-jump-to-tag (arg)
  "Jump to tag under point.
If called with a prefix argument, provide a prompt
for specifying the tag."
  (interactive "P")
  (cond
   ((fboundp 'xref-find-definitions)
	(let ((xref-prompt-for-identifier arg))
	  (call-interactively #'xref-find-definitions)))
   ((fboundp 'find-tag)
	(if arg (call-interactively #'find-tag)
	  (let ((tag (funcall (or find-tag-default-function
							  (get major-mode 'find-tag-default-function)
							  #'find-tag-default))))
		(unless tag (user-error "No tag candidate found around point"))
		(find-tag tag))))))

(defun my-async-shell-command (command)
  "Execute string COMMAND asynchronously."
  (let* ((proc (start-process "Shell"
							  nil
							  shell-file-name
							  shell-command-switch command)))
	(set-process-sentinel proc `(lambda (process signal)
								  (let* ((status (process-status process)))
									(when (memq status '(exit signal))
									  (unless (string= (substring signal 0 -1) "finished")
										(message "Failed to run \"%s\"." ,command))))))))

(defun my-show-build-configration()
  "Show build-configrations."
  (interactive)
  (if (not (derived-mode-p 'eshell-mode) )
	  (progn
		(switch-to-buffer "*Messages*")
		(message "-------------------------")
		(message "Build-config:")
		(message "\[%s\]" system-configuration-options)
		(message " ")
		(message "Config-features:")
		(message "\[%s\]" system-configuration-features)
		(message "-------------------------")
		)
	(progn
	  (eshell-printn "Build-config:")
	  (eshell-print (format "\[%s\]\n" system-configuration-options))
	  (eshell-printn " ")
	  (eshell-printn "Config-features:")
	  (eshell-print (format "\[%s\]\n" system-configuration-features))
	  (eshell-print "-------------------------"))
	)
  )

(defun my-trans-window-split()
  "Trans between horizonal and vertical;; See emacs.stackexchange 46664;;"
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
										 (car next-win-edges))
									 (<= (cadr this-win-edges)
										 (cadr next-win-edges)))))
			 (splitter
			  (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  'split-window-horizontally
				'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)
		  (if this-win-2nd (other-window 1))))))
;; }}

;; see emacs-china.org/t/topic/7568
(define-translation-table 'my-fullwidth-to-halfwidth
  (let ((table (make-char-table 'translation-table)))
	(cl-loop for fullwidth from #xFF01 to #xFF5E
			 for halfwidth from #x21 to #x7E
			 do (aset table fullwidth halfwidth))
	table))

(define-translation-table 'my-halfwidth-to-fullwidth
  (let ((table (make-char-table 'translation-table)))
	(cl-loop for fullwidth from #xFF01 to #xFF5E
			 for halfwidth from #x21 to #x7E
			 do (aset table halfwidth fullwidth))
	table))

;; {{ some other functions
;; for every repl
(defun my-line-change()
  (interactive)
  (insert "\n")
  )

;; non the same in tui or gui, so bind both
(global-set-key (kbd "<C-return>") 'my-line-change)
(global-set-key  [(control return)] 'my-line-change)

;;; Definers
;; copy from doom
(defun my-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

;;{{ string and file actions
(defun my-get-string-from-file (file)
  "Return FILE's content."
  (with-temp-buffer
	(insert-file-contents file)
	(buffer-string)))

(defun my-read-lines (file)
  "Return a list of lines of FILE."
  (split-string (my-get-string-from-file file) "\n" t))

(defun my-write-to-file (str file)
  "Write STR to FILE."
  (with-temp-buffer
	(insert str)
	(write-file (file-truename file))))

(defun my-write-to-missing-file (str file)
  "Write STR to FILE if it's missing."
  (unless (file-exists-p file)
	(my-write-to-file str file)))

(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
		(pos 0)
		(group (or group 0)))
	(while (string-match regex str pos)
	  (push (match-string group str) result)
	  (setq pos (match-end group)))
	result))

(defun path-in-directory-p (file directory)
  "FILE is in DIRECTORY."
  (let* ((pattern (concat "^" (file-name-as-directory directory))))
	(if (string-match-p pattern file) file)))

(defun my-prepare-candidate-fit-into-screen (s)
  (let* ((w (frame-width))
		 ;; display kill ring item in one line
		 (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s)))
	;; strip the whitespace
	(setq key (replace-regexp-in-string "^[ \t]+" "" key))
	;; fit to the minibuffer width
	(if (> (length key) w)
		(setq key (concat (substring key 0 (- w 4)) "...")))
	(cons key s)))

(defun my-buffer-too-big-p ()
  ;; 5000 lines
  (> (buffer-size) (* 5000 80)))

(defun my-file-too-big-p (file)
  (> (nth 7 (file-attributes file))
	 (* 5000 64)))

(defun my-is-buffer-file-temp ()
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file."
  (interactive)
  (let* ((f (buffer-file-name)) (rlt t))
	(cond
	 ((not f)
	  ;; file does not exist at all
	  ;; org-babel edit inline code block need calling hook
	  (setq rlt nil))
	 ((string-match (concat "^" temporary-file-directory) f)
	  ;; file is create from temp directory
	  (setq rlt t))
	 ((and (string-match "\.html$" f)
		   (file-exists-p (replace-regexp-in-string "\.html$" ".org" f)))
	  ;; file is a html file exported from org-mode
	  (setq rlt t))
	 (t
	  (setq rlt nil)))
	rlt))


(defun my-delete-selected-region ()
  "Delete selected region,and insert into `kill-ring'."
  (interactive)
  (when (region-active-p)
	(delete-region (region-beginning) (region-end))))

(defun my-remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun my-delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
							 (file-name-nondirectory buffer-file-name)))
	(delete-file (buffer-file-name))
	(kill-this-buffer)))

(defun my-rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
		(filename (buffer-file-name)))
	(unless filename
	  (error "Buffer '%s' is not visiting a file!" name))
	(if (get-buffer new-name)
		(message "A buffer named '%s' already exists!" new-name)
	  (progn
		(rename-file filename new-name 1)
		(rename-buffer new-name)
		(set-visited-file-name new-name)
		(set-buffer-modified-p nil)))))
;; }}
(defun my-random-alnum ()
  "Return a random alphanic from a-z and 0-9."
  (let* ((alnum "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
		 (i (% (abs (random)) (length alnum))))
	(substring alnum i (1+ i))))

(defun my-generate-random-string (&optional num)
  "Return a random string of length NUM(default 5)."
  (let ((num (or num 5))
		(outString ""))
	(setq outString "")
	(dotimes (i num)
	  (setq outString (concat (my-random-alnum) outString)))
	outString))

(defmacro my-let* (bindings &body body)
  (let ((form body))
	(dolist (clues (nreverse bindings))
	  (setf form (list (nconc `(let (,clues)) form))))
	(car form)))

;; check math functions in doc `number', it's better than gnu manuel itself!
;; float-pi and float-e could be useful.
(defun math-fact (input)
  "Return factorial of `INPUT'."
  (cl-labels ((my-inside-fact (to-multiply next-gen)
							  (if (< to-multiply 1)
								  next-gen
								(my-inside-fact (- to-multiply 1) (* next-gen to-multiply)))))
	(my-inside-fact (- input 1) input)))

(defun math-pow (x y)
  "A wrap for `expt'."
  (expt x y))

;; see it in emacs-wiki!
(defun my-fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing parent directories with their initial characters to try to get the character length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
		 (len (+ (1- (length components))
				 (cl-reduce '+ components :key 'length)))
		 (str ""))
	(while (and (> len max-len)
				(cdr components))
	  (setq str (concat str
						(cond ((= 0 (length (car components))) "/")
							  ((= 1 (length (car components)))
							   (concat (car components) "/"))
							  (t
							   (if (string= "."
											(string (elt (car components) 0)))
								   (concat (substring (car components) 0 2)
										   "/")
								 (string (elt (car components) 0) ?/)))))
			len (- len (1- (length (car components))))
			components (cdr components)))
	(concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

;; https://archive.casouri.cat/note/2020/painless-transition-to-portable-dumper/index.html
(defun luna-dump ()
  "Dump Emacs."
  (interactive)
  (let ((buf "*dump process*"))
    (make-process
     :name "dump"
     :buffer buf
     :command (list "emacs" "--batch" "-q"
                    "-l" (expand-file-name "dump.el"
                                           user-emacs-directory)))
    (display-buffer buf)))

(defmacro +measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06fs" (float-time (time-since time)))))

(defun my-space ()
  (interactive)
  (insert " "))

(defun my-get-current-directory (&optional buffer)
  "Get current or BUFFER directory."
  (if buffer
      (with-current-buffer buffer
	(file-name-directory (or (buffer-file-name) default-directory)))
    (file-name-directory (or (buffer-file-name) default-directory))))

;; Nice implement of cl-let.
(cl-defmacro my-when-let (binding &rest body)
  (declare (indent 1))
  `(let ((,(cl-first binding) ,(cl-second binding)))
     (when ,(cl-first binding)
       ,@body)))

(defun strip-duplicates (list)
  "Remove duplicates in LIST."
  (let ((new-list nil))
    (while list
      (when (and (car list) (not (member (car list) new-list)))
        (setq new-list (cons (car list) new-list)))
      (setq list (cdr list)))
    (nreverse new-list)))

(defun range (min max &optional step)
  (let ((step 1))
	(when (<= min max)
	  (cons min (range (+ min step) max step)))))

(defun my-insert-wave-link ()
  "Insert `~'."
  (interactive)
  (insert "~"))

(provide 'init-func)
;;; init-func.el ends here.
