;;; More than editor.
;; {{ Note
;; encrypt
(with-eval-after-load 'epa
  (setq epa-file-select-keys nil))


(after! rcirc
  (add-hook 'rcirc-mode-hook 'rcirc-track-minor-mode)
  (add-hook 'rcirc-mode-hook 'rcirc-omit-mode)
  (setq rcirc-always-use-server-buffer-flag t
		rcirc-authenticate-before-join t
		rcirc-auto-authenticate-flag t
		rcirc-fill-column #'window-text-width
		rcirc-kill-channel-buffers t
		))

;; {{ calendar
;; 分别是妇女节、植树节、劳动节、青年节、儿童节、教师节、国庆节、程序员节、双11
(with-eval-after-load 'calendar
  (setq holiday-local-holidays `((holiday-fixed 3 8  "Women's Day")
								 (holiday-fixed 3 12 "Arbor Day")
								 ,@(cl-loop for i from 1 to 3
											collect `(holiday-fixed 5 ,i "International Workers' Day"))
								 (holiday-fixed 5 4  "Chinese Youth Day")
								 (holiday-fixed 6 1  "Children's Day")
								 (holiday-fixed 9 10 "Teachers' Day")
								 ,@(cl-loop for i from 1 to 7
											collect `(holiday-fixed 10 ,i "National Day"))
								 (holiday-fixed 10 24 "Programmers' Day")
								 (holiday-fixed 11 11 "Singles' Day")))

  ;; 分别是世界地球日、世界读书日、俄罗斯的那个程序员节
  (setq holiday-other-holidays '((holiday-fixed 4 22 "Earth Day")
								 (holiday-fixed 4 23 "World Book Day")
								 (holiday-sexp '(if (or (zerop (% year 400))
														(and (% year 100) (zerop (% year 4))))
													(list 9 12 year)
												  (list 9 13 year))
											   "World Programmers' Day")))
  (setq calendar-chinese-all-holidays-flag t
		calendar-mark-holidays-flag t
		calendar-mark-diary-entries-flag nil
		;; Prefer +0800 over CST
		calendar-time-zone-style 'numeric
		;; year/month/day
		calendar-date-style 'iso))

;;{{ dired
(with-eval-after-load 'dired

  ;; wsl tune
  (when *wsl*
	(defmacro wsl--open-with (id &optional app dir)
	  `(defun ,(intern (format "wsl/%s" id)) ()
		 (interactive)
		 (wsl-open-with ,app ,dir)))

	(defun wsl-open-with (&optional app-name path)
	  "Send PATH to APP-NAME on WSL."
	  (interactive)
	  (let* ((path (expand-file-name
					(replace-regexp-in-string
					 "'" "\\'"
					 (or path (if (derived-mode-p 'dired-mode)
								  (dired-get-file-for-visit)
								(buffer-file-name)))
					 nil t)))
			 (command (format "%s `wslpath -w %s`" (shell-quote-argument app-name) path)))
		(shell-command-to-string command)))
	
	(wsl--open-with open-in-default-program "explorer.exe" buffer-file-name)
	(wsl--open-with reveal-in-explorer "explorer.exe" default-directory))

  (defun my-replace-dired-base (base)
	"Change file name in `wdired-mode'"
	(let* ((fp (dired-file-name-at-point))
		   (fb (file-name-nondirectory fp))
		   (ext (file-name-extension fp))
		   (dir (file-name-directory fp))
		   (nf (concat base "." ext)))
	  (when (yes-or-no-p (format "%s => %s at %s?"
								 fb nf dir))
		(rename-file fp (concat dir nf)))))
  (defun my-extract-mp3-from-video ()
	"Extract mp3 from current video file using ffmpeg."
	(interactive)
	(let* ((video-file (file-name-nondirectory (dired-file-name-at-point)))
		   (params (split-string (string-trim (read-string "Please input start-second [total seconds] (e.g, \"6 10\" or \"05:30 5\") or just press enter: "))
								 " +"))
		   (start (car params))
		   (total (if (eq (length params) 1) "5" (nth 1 params)))
		   cmd)
	  (cond
	   ((string= start "")
		;; extract audio to MP3 with sample rate 44.1Khz (CD quality), stereo, and 2 channels
		(setq cmd (format "ffmpeg -i \"%s\" -vn -ar 44100 -ac 2 -ab 192 -f mp3 \"%s\""
						  video-file
						  (concat (file-name-base video-file) ".mp3"))))
	   (t
		(setq cmd (format "ffmpeg -i \"%s\" -vn -ss %s -t %s -acodec copy \"%s\""
						  video-file
						  start
						  total
						  (format "%s-%s-%s.mp3" (file-name-base video-file) start total)))))
	  (shell-command (concat cmd " &"))))

  (defun my-extract-mkv-subtitle ()
	"Use mkvtoolnix to extract mkv subtitle."
	(interactive)
	(let* ((file (file-name-nondirectory (dired-file-name-at-point)))
		   (ext (file-name-extension file))
		   (default-directory (file-name-directory (dired-file-name-at-point)))
		   lines
		   trunks
		   track-number)
	  (cond
	   ((not (string= "mkv" ext))
		(message "Only mkv files can be processed."))
	   ((not (executable-find "mkvextract"))
		("Please install mkvtoolnix."))
	   (t
		;; split output into trunks
		(setq trunks (split-string (shell-command-to-string (format "mkvinfo \"%s\"" file))
								   "| ?\\+ [A-Z][^\n]+[\n]*"))
		;; only interested english subtitle trunk
		(setq trunks (delq nil (mapcar
								(lambda (trunk)

								  (when (and (string-match "Track type: subtitles" trunk)
											 (or (not (string-match "Language: " trunk))
												 (string-match "Language: eng" trunk)))
									trunk))
								trunks)))
		(when (and (> (length trunks) 0)
				   (string-match "Track number: \\([0-9]+\\)" (car trunks)))

		  ;; only extract the track number from the first truck
		  (setq track-number (1- (string-to-number (match-string 1 (car trunks)))))
		  (shell-command (format "mkvextract tracks \"%s\" %s:\"%s.srt\" > /dev/null 2>&1"
								 file
								 track-number
								 (file-name-base file))))))))

  (defun my-record-wav-by-mp3 ()
	"Record a wav using meta data from current mp3 file."
	(interactive)
	(let* ((mp3-file (file-name-nondirectory (dired-file-name-at-point)))
		   (base (file-name-base mp3-file))
		   (params (split-string base  "-"))
		   (output-file (concat base ".wav"))
		   (total (string-to-number (nth (1- (length params)) params)))
		   cmd)
	  (if (= total 0) (setq total 4))
	  (setq cmd (format "arecord -fdat -d %s \"%s\""
						total
						output-file))
	  (message "Start recording %s seconds wav ..." total)
	  (my-async-shell-command cmd)))

  (defun my-play-both-mp3-and-wav ()
	"Play wav and mp3."
	(interactive)
	(let* ((audio-file (file-name-nondirectory (dired-file-name-at-point)))
		   (base (file-name-base audio-file))
		   (ext (file-name-extension audio-file) )
		   (cmd (format "mplayer -quiet \"%s\" \"%s\""
						audio-file
						(concat base "." (if (string= ext "mp3") "wav" "mp3")))))
	  (my-async-shell-command cmd)))

  (defun my-stop-player ()
	(interactive)
	(my-async-shell-command "killall -9 mplayer")
	)

  (defun my-copy-file-info (fn)
	(message "%s => clipboard & yank ring"
			 (copy-yank-str (funcall fn (dired-file-name-at-point)))))

(defun my-ediff-files ()
  "@see https://oremacs.com/2017/03/18/dired-ediff/."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (wnd (current-window-configuration)))
    (cond
     ((<= (length files) 2)
      (let* ((file1 (car files))
             (file2 (if (cdr files)
                        (cadr files)
                      (read-file-name
                       "file: "
                       (dired-dwim-target-directory)))))
        (if (file-newer-than-file-p file1 file2)
            (ediff-files file2 file1)
          (ediff-files file1 file2))
        (add-hook 'ediff-after-quit-hook-internal
                  (lambda ()
                    (setq ediff-after-quit-hook-internal nil)
                    (set-window-configuration wnd)))))
     (t
      (error "no more than 2 files should be marked")))))

  (defhydra hydra-dired (:color blue :exit nil)
	"
^Misc^                      ^File^              ^Copy Info^
-----------------------------------------------------------------
[_vv_] video2mp3            [_R_] Move          [_pp_] Path
[_aa_] Record by mp3        [_cf_] New          [_nn_] Name
[_zz_] Play wav&mp3         [_rr_] Rename       [_bb_] Base
[_ee_] Mkv => Srt           [_ff_] Find         [_dd_] directory
[_sa_] Fetch all subtitles  [_C_]  Copy
[_s1_] Fetch on subtitle    [_rb_] Change base  [_m_] mark
[_vv_] Video => Mp3         [_df_] Diff 2 files [_M_] unmark
[_aa_] Recording Wav        [_d_] Delete
[_+_] Create directory      [_D_] Delete Marked
[_S_]  Stop play


"
	("sa" (my-fetch-subtitles))
	("s1" (my-fetch-subtitles (dired-file-name-at-point)))
	("pp" (my-copy-file-info 'file-truename))
	("nn" (my-copy-file-info 'file-name-nondirectory))
	("bb" (my-copy-file-info 'file-name-base))
	("dd" (my-copy-file-info 'file-name-directory))
	("rb" (my-replace-dired-base (car kill-ring)))
	("vv" my-extract-mp3-from-video)
	("ee" my-extract-mkv-subtitle)
	("aa" my-record-wav-by-mp3)
	("zz" my-play-both-mp3-and-wav)
	("C" dired-do-copy)
	("R" dired-do-rename)
	("S" my-stop-player)
	("cf" find-file)
	("df" my-ediff-files)
	("rr" dired-toggle-read-only)
	("ff" (lambda (regexp)
			(interactive "sMatching regexp: ")
			(find-lisp-find-dired default-directory regexp)))
	("+" dired-create-directory)
	("d"  dired-do-delete)
	("D"  dired-do-flagged-delete)
	("m"  dired-mark)
	("M"  dired-unmark)
	("j"  dired-next-line)
	("k"  dired-previous-line)
	("q" nil)))

(defun dired-mode-hook-hydra-setup ()
    (local-set-key (kbd "M-o") 'hydra-dired/body))
(add-hook 'dired-mode-hook 'dired-mode-hook-hydra-setup)


(provide 'init-life)
;;; init-life.el ends here.
