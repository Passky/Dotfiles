;; Mostly copy from https://github.com/casouri/lunarymacs
;; it's awesome!!!
;; for consistency I gonna rename some of 'em

;; DONE: Make bind like `"foo" '(lambda () (foo ...))' usable
;; Solution : warp by a lambda
;; bind-key is from https://github.com/jwiegley/use-package/blob/master/bind-key.el

;; NOTE: Make SPC leader usable everywhere,by edit it in evli-collection
;; NOTE: evil-collection captured it most of time.

;;; Code:

(require 'pcase)

(defmacro my-lambda (&rest body)
  "Wrap BODY in an interactive lambda."
  (declare (indent defun))
  `(lambda () (interactive)
	 ,@body))

(defvar my-key-preset-alist nil
  "Stores defined presets.")

(defvar my-key-override-mode-map (make-sparse-keymap)
  "One map to rule them all.")

(defvar my-key-override-map-alist
  `(my-key-override-mode . ,my-key-override-mode-map)
  "Alist put into `emulation-mode-map-alists' to override all other maps.")

(define-minor-mode my-key-override-mode
  "Minor mode used to activate our override keymap."
  :global t
  :lighter ""
  :group 'emacs
  :keymap 'my-key-override-mode-map)

(defvar my-key-postponed-alist nil
  "An alist of (map . ((key def) (key def))).
When we define a key in a map that’s not defined yet, we push the
definition to this alist.  When a new file is loaded, we check for
each map and see if we can now define the bindings for that map.")

(defun my-key-define-postponed-binding (_)
  "Define postponed bindings in ‘my-key-postponed-alist’."
  (dolist (map (mapcar #'car my-key-postponed-alist))
	(when (boundp map)
	  (pcase-dolist (`(,key ,def)
					 (alist-get map my-key-postponed-alist))
		;; (bind-key key def (symbol-value map)))
		(define-key (symbol-value map) key (my-lambda def)))
	  (setf (alist-get map my-key-postponed-alist) nil))))

(defun my-key-def-preset (preset &rest args)
  "Define PRESET as ARGS.

ARGS can be anything valid in `my-def-key'.

If you define :leader as

	(my-key-def-preset :leader
	 :keymaps 'override
	 :prefix \"C-SPC\")

Then

	(my-def-key
	 :leader
	 KEY DEF)

is equivalent to

	(my-def-key
	 :keymaps 'override
	 :prefix \"C-SPC\"
	 KEY DEF)"
  (declare (indent 1))
  (setf (alist-get preset my-key-preset-alist) args))

(defun my-key-normalize (prefix key)
  "Normalize KEY and PREFIX with `kbd' and combine them.
However, if KEY is [remap ...] or [t], don’t prepend PREFIX to it."
  ;; Normalize KEY and PREFIX with `kbd'.
  (if (stringp key) (setq key (kbd key)))
  (if (and prefix (stringp prefix)) (setq prefix (kbd prefix)))
  ;; Result of ‘kbd’ can be either string or vector,
  ;; now we normalize KEY and PREFIX to vectors.
  (if (stringp key) (setq key (string-to-vector key)))
  (if (and prefix (stringp prefix))
	  (setq prefix (string-to-vector prefix)))
  ;; When do we simply return KEY without PREFIX: either PREFIX is
  ;; nil, or KEY has special meaning ([remap ...] or [t]).
  (if (or (not prefix) (and (vectorp key) (memq (aref key 0) '(t remap))))
	  key
	(vconcat prefix key)))

(defun my-key-define (key def map-list prefix)
  "Define KEY to DEF.
Define KEY in all the maps in MAP-LIST, using PREFIX as prefix.
MAP-LIST and PREFIX can be nil.
If MAP-LIST is nil, only define in `global-map'."
  (let ((map-list (or map-list (list 'global-map)))
		(key (my-key-normalize prefix key))
		;; If DEF is (STRING . DEFN), we use STRING as it’s description.
		(desc (car-safe def)))
	(when desc
	  (with-eval-after-load 'which-key
		(which-key-add-key-based-replacements
		  (key-description key) desc)))
	(dolist (map map-list)
	  (let ((map (if (eq map 'override) 'my-key-override-mode-map map)))
		(if (boundp map)
			(define-key (symbol-value map) key def)
		  ;; (bind-key key def (symbol-value map))
		  (push (list key def)
				(alist-get map my-key-postponed-alist))
		  (add-hook 'after-load-functions
					#'my-key-define-postponed-binding))))))

(defun my-def-key (&rest args)
  "Define key.

The :keymaps and :prefix modifiers specifies the keymaps and
prefix key for KEY DEF pairs below them.  Modifiers only affect
the definitions after them, and their effect lasts until another
modifier overrides them. For example, to define KEY1 in MAP1 and
KEY2 in MAP2:

  (my-def-key
   :keymaps 'MAP1
   KEY1 DEF1
   :keymaps 'MAP2n
   KEY2 DEF2)

Unlike in `define-key', MAP is the symbol of a keymap, rather than
the keymap itself. MAP can also be nil, which is interpreted as
`global-map', or 'override, which is interpreted as the override
keymap defined by my-key, or a list of these three forms. KEY
and DEF can be anything that `define-key' accepts. `kbd' is
automatically added to KEY but not DEF.

You can also use preset modifiers defined by `my-key-def-preset'.
They are basically macros that expand to other modifiers.

Use :clear to reset all modifier effects. :--- is an alias for :clear.

ARGS.

\(fn [:keymaps MAPS] [:prefix PREFIX] [:clear] KEY DEF ...)"
  (my-key-override-mode)
  (condition-case nil
	  (let (arg map-list prefix)
		(while args
		  (setq arg (pop args))
		  (pcase arg
			(:keymaps
			 (let ((map (pop args)))
			   (cond ((symbolp map) (setq map-list (list map)))
					 ((proper-list-p map) (setq map-list map))
					 (t (error "Invalid argument for :keymaps command: %s"
							   map)))))
			(:prefix
			 (setq prefix (pop args)))
			((or :clear :---) (setq prefix nil
									map-list nil))
			((pred keywordp)
			 (when-let ((preset (alist-get arg my-key-preset-alist)))
			   (setq args (append preset args))))
			(_ (let ((key arg)
					 (def (pop args)))
				 (my-key-define key def map-list prefix)
				 )))))
	(setting-constant (error "Not enough arguments"))))





;;; fully copy from key-chord
;; since I do not catch upstream,just rename instead of alias
;;; https://github.com/emacsorphanage/key-chord

(defgroup my-keychord nil
  "Map pairs of simultaneously pressed keys to commands."
  :group 'bindings)

(defcustom my-keychord-two-keys-delay 0.3
  "Max time delay between two key press to be considered a key chord."
  :type 'float)

(defcustom my-keychord-one-key-delay 0.3
  "Max time delay between two press of the same key to be considered a key chord.
This should normally be a little longer than `my-keychord-two-keys-delay'."
  :type 'float)

(defcustom my-keychord-in-macros t
  "If nil, don't expand key chords when executing keyboard macros.

If non-nil, expand chord sequenses in macros, but only if a
similar chord was entered during the last interactive macro
recording. (This carries a bit of guesswork. We can't know for
sure when executing whether two keys were typed quickly or slowly
when recorded.)"
  :type 'boolean)

;; Internal vars
(defvar my-keychord-mode nil)

;; Shortcut for my-keychord-input-method: no need to test a key again if it
;; didn't matched a chord the last time. Improves feedback during autorepeat.
(defvar my-keychord-last-unmatched nil)

;; Macro heuristics: Keep track of which chords was used when the last macro
;; was defined. Or rather, only the first-char of the chords. Only expand
;; matching chords during macro execution.
(defvar my-keychord-in-last-kbd-macro nil)
(defvar my-keychord-defining-kbd-macro nil)

(define-minor-mode my-keychord-mode
  "Map pairs of simultaneously pressed keys to commands.

See functions `my-keychord-define-global', `my-keychord-define-local',
and `my-keychord-define' and variables `my-keychord-two-keys-delay'
and `my-keychord-one-key-delay'."
  :global t
  (setq input-method-function
		(and my-keychord-mode
			 'my-keychord-input-method)))

(defun my-keychord-define-global (keys command)
  "Define a my-keychord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the my-keychord is removed.

Note that KEYS defined locally in the current buffer will have
precedence."
  (interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (my-keychord-define (current-global-map) keys command))

(defun my-keychord-unset-global (keys)
  "Remove global my-keychord of the two keys in KEYS."
  (interactive "sUnset key chord globally (2 keys): ")
  (my-keychord-define (current-global-map) keys nil))

(defun my-keychord-define (keymap keys command)
  "Define in KEYMAP, a my-keychord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the my-keychord is removed."
  (if (/= 2 (length keys))
	  (error "my-keychord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255
  ;; for those.
  (let ((key1 (logand 255 (aref keys 0)))
		(key2 (logand 255 (aref keys 1))))
	(if (eq key1 key2)
		(define-key keymap (vector 'my-keychord key1 key2) command)
	  (define-key keymap (vector 'my-keychord key1 key2) command)
	  (define-key keymap (vector 'my-keychord key2 key1) command))))

(defun my-keychord-lookup-key1 (keymap key)
  "Like lookup-key but no third arg and no numeric return value."
  (let ((res (lookup-key keymap key)))
	(and (not (numberp res))
		 res)))

(defun my-keychord-lookup-key (key)
  "Lookup KEY in all current key maps."
  (let ((maps (current-minor-mode-maps))
		res)
	(while (and maps (not res))
	  (setq res (my-keychord-lookup-key1 (car maps) key))
	  (setq maps (cdr maps)))
	(or res
		(and (current-local-map)
			 (my-keychord-lookup-key1 (current-local-map) key))
		(my-keychord-lookup-key1 (current-global-map) key))))

(defun my-keychord-describe ()
  "List key chord bindings in a help buffer.

Two key chords will be listed twice and there will be Prefix
Commands. Please ignore that."
  (interactive)
  (describe-bindings [my-keychord]))

(defun my-keychord-input-method (first-char)
  "Input method controlled by key bindings with the prefix `my-keychord'."
  (cond
   ((and (not (eq first-char my-keychord-last-unmatched))
		 (my-keychord-lookup-key (vector 'my-keychord first-char)))
	(let ((delay (if (my-keychord-lookup-key
					  (vector 'my-keychord first-char first-char))
					 my-keychord-one-key-delay
				   my-keychord-two-keys-delay)))
	  (cond ((if executing-kbd-macro
				 (not (memq first-char my-keychord-in-last-kbd-macro))
			   (when (bound-and-true-p eldoc-mode)
				 (eldoc-pre-command-refresh-echo-area))
			   (sit-for delay 'no-redisplay))
			 (setq my-keychord-last-unmatched nil)
			 (list first-char))
			(t ; input-pending-p
			 (let* ((input-method-function nil)
					(next-char (read-event))
					(res (vector 'my-keychord first-char next-char)))
			   (cond ((my-keychord-lookup-key res)
					  (setq my-keychord-defining-kbd-macro
							(cons first-char my-keychord-defining-kbd-macro))
					  (list 'my-keychord first-char next-char))
					 (t ;put back next-char and return first-char
					  (setq unread-command-events
							(cons next-char unread-command-events))
					  (when (eq first-char next-char)
						(setq my-keychord-last-unmatched first-char))
					  (list first-char))))))))
   (t ; no my-keychord keymap
	(setq my-keychord-last-unmatched first-char)
	(list first-char))))

(defun my-keychord--start-kbd-macro (_append &optional _no-exec)
  (setq my-keychord-defining-kbd-macro nil))
(advice-add 'start-kbd-macro :after #'my-keychord--start-kbd-macro)

(defun my-keychord--end-kbd-macro (&optional _repeat _loopfunc)
  (setq my-keychord-in-last-kbd-macro my-keychord-defining-kbd-macro))
(advice-add 'end-kbd-macro :after #'my-keychord--end-kbd-macro)

(provide 'init-mykey.el)
;;; init-mykey.el ends here
