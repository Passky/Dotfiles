;;{{ font set
(defun x-display-ppi ()
  "Return the PPI of the display.

Modified from `org--get-display-dpi'.

The function assumes that the display has the same pixel width in
the horizontal and vertical directions."
  (if (display-graphic-p)
      (let ((mm-h (cl-caddr (assoc 'mm-size (frame-monitor-attributes)))))
        (round (/ (display-pixel-height)
    	          (/ mm-h 25.4))))
    (error "Attempt to calculate the dpi of a non-graphic display")))

;; Ctrl + scrool dowm/up to scale font
(global-set-key [C-kp-add] 'text-scale-increase)
(global-set-key [C-kp-subtract] 'text-scale-decrease)

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun my-dos-to-unix ()
  "Not exactly but it's easier to remember."
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )

;; Avoid error for emacs compiling with non x toolkit support.
;; from centaur https://github.com/seagle0128/.emacs.d
(when (and (functionp 'set-face-attribute) (functionp 'set-fontset-font))
  ;; cute frame title
  (setq frame-title-format   '("%b  (●'◡'●)ﾉ♥")
		icon-title-format frame-title-format)
  (cl-loop for font in '(
						 "jetbrains mono"
						 "Consolas"
						 "Fira Code"
						 "Source Code Variable"
						 "Menlo" "Monaco"
						 "SF Mono" "Hack"
						 "DejaVu Sans Mono"
						 )
		   when (font-installed-p font)
		   return (progn (set-face-attribute 'default nil
											 :font font
											 :height (cond (*darwin* 125)
														   (*win64* 110)
														   (t 100)))
						 (set-face-attribute 'mode-line nil :font font)
						 (set-face-attribute 'mode-line-inactive nil :font font)
						 ))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji"
						 "Segoe UI Symbol"
						 "Noto Color Emoji"
						 "Symbola"
						 "Symbol")
		   when (font-installed-p font)
		   return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("Source Han Serif SC"
						 "Source Han Sans SC"
						 "WenQuanYi Micro Hei"
						 "WenQuanYi Yahei"
						 "Microsoft Yahei")
		   when (font-installed-p font)
		   return (set-fontset-font t '(#x4e00 . #x9fff) font)))
;;}}
(provide 'init-gui)
;;; init-gui.el ends here
