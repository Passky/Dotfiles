;; from centaur
(defconst my-server-tested? nil
  "If servers have been tested?")

(defconst my-package-archives-alist
  (let* ((proto (if (or (not (boundp 'libgnutls-version)) (= libgnutls-version -1)) "http" "https")))
    `(,(cons 'melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
      ,(cons 'emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
      ,(cons 'netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
      ,(cons 'ustc
             `(,(cons "gnu"   (concat proto "://mirrors.ustc.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.ustc.edu.cn/elpa/melpa/"))))
      ,(cons 'tencent
             `(,(cons "gnu"   (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))))
      ,(cons 'tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))))))

(setq package-archives
	  (alist-get 'ustc my-package-archives-alist))

;; Refer to https://emacs-china.org/t/elpa/11192
(defun my-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.
Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."

  (interactive)
  (let* ((urls (mapcar
                (lambda (url)
                  (concat url "archive-contents"))
                (mapcar #'cdr
                        (mapcar #'cadr
                                (mapcar #'cdr
                                        my-package-archives-alist)))))
         (durations (mapcar
                     (lambda (url)
                       (let ((start (current-time)))
                         (message "Fetching %s..." url)
                         (cond ((executable-find "curl")
                                (call-process "curl" nil nil nil "--max-time" "10" url))
                               ((executable-find "wget")
                                (call-process "wget" nil nil nil "--timeout=10" url))
                               (t (user-error "curl or wget is not found")))
                         (float-time (time-subtract (current-time) start))))
                     urls))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            my-package-archives-alist))))
	(setq my-server-tested? t) ; do not test again
    ;; Return the fastest
    fastest))

;; new feature in emacs27
(defconst my-cache-file (concat my-emacs-d "lisp/autoloads.el")
  "Directory of cache file.")

(setq package-quickstart t
	  package-quickstart-file my-cache-file)

(package-initialize)

;; Bug fix
(when (bound-and-true-p *dump*)
  (load my-cache-file))

(defun my-re-init ()
  "For cross big version update."
  (interactive)
  (delete-file (concat package-quickstart-file "c"))
  (delete-file package-quickstart-file)
  (package-quickstart-refresh))

(defun my-add-package (package &optional no-refresh)
	"Ask elpa to install given PACKAGE."
	(cond
	 ((package-installed-p package)
	  t)
	 ((or (assoc package package-archive-contents) no-refresh)
	  (package-install package))
	 (t
	  (package-refresh-contents)
	  (my-add-package package t))))

;; for native-comp branch
(when (fboundp 'native-compile-async)
  (setq comp-async-jobs-number 7 ;; not to use all cores
		comp-deferred-compilation t
		;; comp-deferred-compilation-black-list '()
		comp-async-report-warnings-errors nil) ; or it will be too annoying
  )
;; 

(my-add-package 'gnu-elpa-keyring-update) ; keep keyring up with time
(my-add-package 'auto-package-update) ; auto-package-update
;; for benchmark
(my-add-package 'esup) ; now use benchmark-init too
(my-add-package 'rainbow-delimiters)
(my-add-package 'ripgrep) ; add support for rg
(my-add-package 'dumb-jump)
(my-add-package 'hydra) ; powerful keyboard support
(my-add-package 'posframe) ; nice graphic work
(my-add-package 'benchmark-init) ; yet another esup


;;auto-package-update
(with-eval-after-load 'auto-package-update
  ;; (setq auto-package-update-interval 4) ;; update every 4 days
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-hide-results t)
  (advice-add  #'auto-package-update-now :after  #'my-re-initialize-package) ; re-initialize after pacakge-update
  )
;; }}

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here.
