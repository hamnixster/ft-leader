(require 'cl-lib)

(defcustom ft-leader-mod-alist
  '((nil . "C-")
    ("SPC" . ""))
  "List of keys and their associated modifier."
  :group 'ft-leader
  :type '(alist))

(defcustom ft-leader-exempt-predicates
  '()
  "List of predicates checked before enabling ft-leader-local-mode.
All predicates must return nil for ft-leader-local-mode to start."
  :group 'ft-leader
  :type '(repeat function))

(defvar ft-leader-global-mode nil
  "Activate GLeader mode on all buffers?")

(defvar ft-leader-no-modifier-top-level-command 'ft-leader-mode-deactivate)
(defvar ft-leader-special-map nil)
(defvar ft-leader-special-command nil)
(defvar ft-leader-special nil)
(defvar ft-leader-maps-alist nil)

(defvar ft-leader-which-key nil)
(defvar ft-leader-which-key-thread nil)
(defvar ft-leader-which-key-received nil)
(defvar ft-leader-which-key-map nil)
(defvar ft-leader-which-key-mod nil)
(defvar ft-current-bindings nil)
(defvar ft-special-bindings nil)
(defvar ft-last-key-string "")
(defvar ft-universal-arg nil)

(defvar ft-leader-local-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'ft-leader-mode-self-insert)
    (dolist (i (number-sequence 32 255))
      (define-key map (vector i) 'ft-leader-mode-self-insert))
    map))

(define-minor-mode ft-leader-local-mode
  "Minor mode for running commands."
  nil " FTLeader" ft-leader-local-mode-map
  (if ft-leader-local-mode
      (run-hooks 'ft-leader-mode-enabled-hook)
    (run-hooks 'ft-leader-mode-disabled-hook)))

(defun ft-leader-ensure-priority-bindings ()
  (unless (and (listp (car emulation-mode-map-alists))
               (car (assoc 'ft-leader-local-mode (car emulation-mode-map-alists))))
    (cl-delete-if
     (lambda (alist)
       (and (listp alist)
            (car (assoc 'ft-leader-local-mode alist))))
     emulation-mode-map-alists)
    (add-to-list
     'emulation-mode-map-alists
     `((ft-leader-local-mode . ,ft-leader-local-mode-map)))))

(defun ft-leader-accept-input ()
  (when ft-leader-which-key
    (setq ft-current-bindings (which-key--get-current-bindings)
          ft-special-bindings
          (mapcar
           (lambda (sp)
             (cons sp (symbol-name (key-binding (read-kbd-macro (format "C-c %c" sp))))))
           ft-leader-special)))
  (ft-leader-ensure-priority-bindings)
  (when ft-leader-which-key
    (setq ft-leader-which-key-thread
          (make-thread 'ft-leader-which-key-top)))
  (setq ft-last-key-string
        (format
         "%s%s"
         (if ft-universal-arg
             (if (consp ft-universal-arg)
                 (apply 'concat
                        (make-list (floor (log (car ft-universal-arg) 4))
                                   "C-u "))
               (format "C-u %s " ft-universal-arg))
           "")
         (cdr (assoc nil ft-leader-mod-alist))))
  (let (message-log-max)
    (message ft-last-key-string)))

(add-hook 'ft-leader-mode-enabled-hook 'ft-leader-accept-input)

(defun ft-leader-mode-sanitized-key-string (key)
  "Convert any special events to textual."
  (cl-case key
    (tab "TAB")
    (?\  "SPC")
    (left "<left>")
    (right "<right>")
    (prior "<prior>")
    (next "<next>")
    (backspace "DEL")
    (return "RET")
    (escape "ESC")
    (?\^\[ "ESC")
    (t (char-to-string key))))

(defun ft-leader-key-string-after-consuming-key (key key-string-so-far)
  "Interpret ft-leader-mode special keys for KEY (consumes more keys if
appropriate). Append to keysequence."
  (let ((key-consumed t) next-modifier next-key)
    (setq next-modifier
          (cond
           ((and
             (stringp key)
             (not (eq nil (assoc key ft-leader-mod-alist)))
             (not (eq nil key)))
            (cdr (assoc key ft-leader-mod-alist)))
           (t
            (setq key-consumed nil)
            (cdr (assoc nil ft-leader-mod-alist))
            )))
    (setq next-key
          (if key-consumed
              (progn
                (setq ft-last-key-string
                      (format
                       "%s%s"
                       (if ft-universal-arg
                           (if (consp ft-universal-arg)
                               (apply 'concat
                                      (make-list (floor (log (car ft-universal-arg) 4))
                                                 "C-u "))
                             (format "C-u %s " ft-universal-arg))
                         "")
                       (format
                        "%s%s"
                        (if key-string-so-far (format "%s " key-string-so-far) "")
                        next-modifier)))
                (let (message-log-max)
                  (message ft-last-key-string))
                (when ft-leader-which-key
                  (setq ft-leader-which-key-map
                        (if key-string-so-far
                            (key-binding (read-kbd-macro key-string-so-far t))
                          nil))
                  (setq ft-leader-which-key-mod (cdr (assoc key ft-leader-mod-alist)))
                  (setq ft-leader-which-key-thread (make-thread 'ft-leader-which-key-with-map))
                  )
                (ft-leader-mode-sanitized-key-string (read-key key-string-so-far)))
            key))

    (when ft-leader-which-key
      (setq ft-leader-which-key-received t)
      (let ((which-key-inhibit t))
        (which-key--hide-popup-ignore-command)))

    (if key-string-so-far
        (concat key-string-so-far " " next-modifier next-key)
      (concat next-modifier next-key))))

(defun ft-leader-mode-lookup-command (key-string)
  "Execute extended keymaps such as C-c, or call command.
KEY-STRING is the command to lookup."
  (let* ((key-vector (read-kbd-macro key-string t))
         (binding (key-binding key-vector)))
    (cond ((commandp binding)
           (setq last-command-event (aref key-vector (- (length key-vector) 1)))
           (ft-leader-mode-deactivate)
           (setq ft-last-key-string
                 (format
                  "%s%s"
                  (if ft-universal-arg
                      (if (consp ft-universal-arg)
                          (apply 'concat
                                 (make-list (floor (log (car ft-universal-arg) 4))
                                            "C-u "))
                        (format "C-u %s " ft-universal-arg))
                    "")
                  key-string))
           (let (message-log-max)
             (message ft-last-key-string))
           binding)
          ((keymapp binding)
           (ft-leader-mode-lookup-key-sequence nil key-string))
          ((eq "" key-string)
           ft-leader-no-modifier-top-level-command)
          (:else
           (message "FTLeader: Unknown key binding for `%s`" key-string)
           'ft-leader-mode-deactivate))))

(defun ft-leader-mode-lookup-key-sequence (&optional key key-string-so-far)
  (interactive)
  (when (and ft-leader-which-key (eq nil key))
    (setq ft-leader-which-key-map (key-binding (read-kbd-macro key-string-so-far t)))
    (setq ft-leader-which-key-mod (cdr (assoc nil ft-leader-mod-alist)))
    (setq ft-leader-which-key-thread (make-thread 'ft-leader-which-key-with-map))
    )

  (when (eq nil key)
    (setq ft-last-key-string
          (format
           "%s%s"
           (if ft-universal-arg
               (if (consp ft-universal-arg)
                   (apply 'concat
                          (make-list (floor (log (car ft-universal-arg) 4))
                                     "C-u "))
                 (format "C-u %s " ft-universal-arg))
             "")
           (format
            "%s%s"
            (if key-string-so-far (format "%s " key-string-so-far) "")
            (cdr (assoc nil ft-leader-mod-alist)))))
    (let (message-log-max)
      (message ft-last-key-string)))

  (let ((sanitized-key
         (if key-string-so-far (ft-leader-mode-sanitized-key-string (or key (read-key key-string-so-far)))
           (ft-leader-mode-sanitized-key-string (or key (read-key key-string-so-far))))))
    (when ft-leader-which-key
      (setq ft-leader-which-key-received t)
      (let ((which-key-inhibit t))
        (which-key--hide-popup-ignore-command)))
    (ft-leader-mode-lookup-command
     (cond ((and key (member key ft-leader-special) (null key-string-so-far))
            (format "C-c %c" key))
           ((and key
                 (string=
                  (ft-leader-mode-sanitized-key-string key)
                  (car (rassq "" ft-leader-mod-alist))
                  )
                 (null key-string-so-far))
            "")
           (:else
            (ft-leader-key-string-after-consuming-key sanitized-key key-string-so-far))))))

(defun ft-leader-mode-upper-p (char)
  "Is the given CHAR upper case?"
  (and (>= char ?A) (<= char ?Z)))

(defun ft-leader-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  (when ft-leader-which-key
    (setq ft-leader-which-key-received t)
    (let ((which-key-inhibit t))
      (which-key--hide-popup-ignore-command)))
  (let* ((command-vec (this-command-keys-vector))
         (initial-key (aref command-vec (- (length command-vec) 1)))
         (binding (ft-leader-mode-lookup-key-sequence initial-key)))
    (when (ft-leader-mode-upper-p initial-key)
      (setq this-command-keys-shift-translated t))
    (setq this-original-command binding)
    (setq this-command binding)
    ;; `real-this-command' is used by emacs to populate
    ;; `last-repeatable-command', which is used by `repeat'.
    (setq real-this-command binding)
    (if (commandp binding t)
        (let* ((current-prefix-arg ft-universal-arg)
               (binding
                (if (and (consp current-prefix-arg)
                         (eq binding 'universal-argument))
                    'universal-argument-more
                  binding)))
          (call-interactively binding))
      (execute-kbd-macro binding))))

(defun ft-leader-passes-predicates-p ()
  "Return non-nil if all `ft-leader-exempt-predicates' return nil."
  (not
   (catch 'disable
     (let ((preds ft-leader-exempt-predicates))
       (while preds
         (when (funcall (car preds))
           (throw 'disable t))
         (setq preds (cdr preds)))))))

(defun ft-leader-mode-deactivate ()
  "Deactivate GLeader mode locally."
  (interactive)
  (when ft-leader-which-key
    (setq ft-leader-which-key-received t)
    (let ((which-key-inhibit t))
      (which-key--hide-popup-ignore-command)))
  (ft-leader-local-mode -1))

(defun ft-leader-mode-exec (arg)
  (interactive "P")
  (setq ft-universal-arg arg)
  (when (and ft-leader-global-mode
             (ft-leader-passes-predicates-p))
    (ft-leader-local-mode 1)))

(defun ft-leader-mode ()
  "Toggle global GLeader mode."
  (interactive)
  (setq ft-leader-global-mode (not ft-leader-global-mode)))

(defun ft-leader-setup-special-maps ()
  (setq ft-leader-maps-alist
        (mapcar
         (lambda (char)
           (cons char (intern (format "C-c %c map" char))))
         ft-leader-special-map)
        ft-leader-special
        (append ft-leader-special-map ft-leader-special-command))
  (dolist (map-key ft-leader-maps-alist)
    (define-prefix-command (cdr map-key)))
  )

(defun ft-leader-bind-special-maps (&optional host-map)
  (dolist (map-key ft-leader-maps-alist)
    (if host-map
        (define-key host-map
          (kbd (format "C-c %c" (car map-key)))
          (cdr map-key))
      (global-set-key
       (kbd (format "C-c %c" (car map-key)))
       (cdr map-key))
      )
    ))

(defun ft-leader-define-key (map-char key-seq command)
  (define-key
    (cdr (assq map-char ft-leader-maps-alist))
    (kbd key-seq)
    command))

(defun ft-leader-define-keys (map-char key-command-alist)
  (dolist (key-command key-command-alist)
    (ft-leader-define-key map-char (car key-command) (cdr key-command))))

(defun ft-leader-which-key-with-map ()
  (let* ((map ft-leader-which-key-map)
         (mod ft-leader-which-key-mod)
         (matcher (format "^<?%s" mod))
         (non-matcher (format "%s.-" matcher)))
    (setq ft-leader-which-key-received nil)
    (sit-for which-key-idle-delay)
    (when (and ft-leader-local-mode
               (not ft-leader-which-key-received)
               (not (which-key--popup-showing-p)))
      (ft-which-key--create-buffer-and-show
       nil map
       (lambda (binding)
         (and (not (string-match-p non-matcher (car binding)))
              (string-match-p matcher (car binding)))
         )
       (lambda (unformatted)
         (dolist (key unformatted)
           (setcar key (replace-regexp-in-string mod "" (car key))))
         (when (string= mod (cdr (assoc nil ft-leader-mod-alist)))
           (setq unformatted
                 (cl-remove-if
                  (lambda (key)
                    (string=
                     (car (rassq "" ft-leader-mod-alist))
                     (car key)))
                  unformatted))
           (setq unformatted
                 (cons
                  (cons
                   (car (rassq "" ft-leader-mod-alist))
                   "no-modifier")
                  unformatted))
           (dolist (mod ft-leader-mod-alist)
             (when (not (or (eq nil (car mod)) (string= "" (cdr mod))))
               (progn
                 (setq unformatted
                       (cl-remove-if
                        (lambda (key) (string= (car mod) (car key)))
                        unformatted))
                 (setq unformatted (cons mod unformatted))))))
         unformatted
         )
       )
      ))
  (let (message-log-max)
    (message ft-last-key-string)))

(defun ft-leader-which-key-top ()
  (setq ft-leader-which-key-received nil)
  (sit-for which-key-idle-delay)
  (when (and ft-leader-local-mode
             (not ft-leader-which-key-received)
             (not (which-key--popup-showing-p)))
    (let* ((mod (cdr (assoc nil ft-leader-mod-alist)))
           (matcher (format "^<?%s" mod))
           (re-literal-matcher
            (format "%s%s"
                    matcher
                    (car (rassq "" ft-leader-mod-alist))))
           (non-matcher (format "%s.-" matcher)))
      (ft-which-key--create-buffer-and-show
       nil nil
       (lambda (binding)
         (and (not (string-match-p non-matcher (car binding)))
              (not (string-match-p re-literal-matcher (car binding)))
              (string-match-p matcher (car binding))
              ))
       (lambda (unformatted)
         (dolist (key unformatted)
           (setcar key (replace-regexp-in-string mod "" (car key))))
         (setq unformatted
               (cl-acons
                (car (rassq "" ft-leader-mod-alist))
                (symbol-name ft-leader-no-modifier-top-level-command)
                unformatted))
         (dolist (mod ft-leader-mod-alist)
           (when (not (or (eq nil (car mod)) (string= "" (cdr mod))))
             (progn
               (setq unformatted
                     (cl-remove-if
                      (lambda (key) (string= (car mod) (car key)))
                      unformatted))
               (setq unformatted (cons mod unformatted)))))
         (dolist (sp ft-leader-special)
           (setq unformatted
                 (cl-remove-if
                  (lambda (key)
                    (string= (ft-leader-mode-sanitized-key-string sp) (car key)))
                  unformatted))
           (when (assq sp ft-special-bindings)
             (setq unformatted
                   (cl-acons
                    (ft-leader-mode-sanitized-key-string sp)
                    (cdr (assq sp ft-special-bindings))
                    unformatted))))
         unformatted))))
  (let (message-log-max)
    (message ft-last-key-string)))

(with-eval-after-load 'which-key
  (defun ft-which-key--create-buffer-and-show
      (&optional prefix-keys from-keymap filter preformat prefix-title)
    "Fill `which-key--buffer' with key descriptions and reformat.
Finally, show the buffer."
    (let ((start-time (current-time))
          (formatted-keys (ft-which-key--get-bindings
                           prefix-keys from-keymap filter preformat))
          (prefix-desc (key-description prefix-keys)))
      (cond ((= (length formatted-keys) 0)
             (setq ft-last-key-string
                   (format
                    "%s%s"
                    (if ft-universal-arg
                        (if (consp ft-universal-arg)
                            (apply 'concat
                                   (make-list (floor (log (car ft-universal-arg) 4))
                                              "C-u "))
                          (format "C-u %s " ft-universal-arg))
                      "")
                    (format "%s which-key: There are no keys to show" ft-last-key-string)))
             )
            ((listp which-key-side-window-location)
             (setq which-key--last-try-2-loc
                   (apply #'which-key--try-2-side-windows
                          formatted-keys prefix-keys prefix-title
                          which-key-side-window-location)))
            (t (setq which-key--pages-obj
                     (which-key--create-pages
                      formatted-keys prefix-keys prefix-title))
               (which-key--show-page)))
      (which-key--debug-message
       "On prefix \"%s\" which-key took %.0f ms." prefix-desc
       (* 1000 (float-time (time-since start-time))))))

  (defun ft-which-key--get-bindings (&optional prefix keymap filter preformat recursive)
    "Collect key bindings.
If KEYMAP is nil, collect from current buffer using the current
key sequence as a prefix. Otherwise, collect from KEYMAP. FILTER
is a function to use to filter the bindings. If RECURSIVE is
non-nil, then bindings are collected recursively for all prefixes."
    (let* ((unformatted
            (cond ((keymapp keymap)
                   (which-key--get-keymap-bindings keymap recursive))
                  (keymap
                   (error "%s is not a keymap" keymap))
                  (ft-current-bindings ft-current-bindings)
                  (t
                   (which-key--get-current-bindings prefix)))))
      (when filter
        (setq unformatted (cl-remove-if-not filter unformatted)))
      (when preformat
        (setq unformatted (funcall preformat unformatted)))
      (when which-key-sort-order
        (setq unformatted
              (sort unformatted which-key-sort-order)))
      (which-key--format-and-replace unformatted prefix recursive)))
  )

(provide 'ft-leader-mode)
