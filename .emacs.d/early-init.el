;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
	  gc-cons-percentage 0.6
	  )

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; In Emacs 27+, package initialization occurs before `user-init-file'
;; so ban it
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore) ; DEPRECATED: removed in emacs28

;; {{ Suppress GUI features
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
; (setq default-frame-alist '((undecorated . t)))
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; full screen
; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Avoid annoying native-comp error
;; (setq warning-minimum-level :error)

;; set file-name-handler-alist to nil during 'require'
;; this is needed because of deferred package loading
(advice-add 'require :around (lambda (orig-fun &rest args)
  (let ((file-name-handler-alist))
    (apply orig-fun args))
))
