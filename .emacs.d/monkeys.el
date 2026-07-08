;; https://github.com/magit/magit/discussions/5602
(defun my/monkeys--magit-dis-5602 nil
  (transient-define-argument magit-commit:--reedit-message ()
  :description "Reedit commit message"
  :class 'transient-option
  :shortarg "-c"
  :argument "--reedit-message="
  :reader #'magit-read-reuse-message
  :history-key 'magit-revision-history)

  (transient-append-suffix 'magit-commit "-C"
    '(magit-commit:--reedit-message))

  (defun magit-read-reuse-message (prompt &optional default history)
    (magit-completing-read prompt (magit-list-refnames)
                           nil nil nil history
                           (or default
                               (magit-commit-at-point)
                               (and (magit-rev-verify "ORIG_HEAD")
                                    "ORIG_HEAD")))))

;; https://github.com/ch11ng/exwm/pull/900
(defun my/monkeys--exwm-pr-900 nil
  (defun exwm-input--on-ButtonPress (data _synthetic)
    "Handle ButtonPress event."
    (let ((obj (make-instance 'xcb:ButtonPress))
          (mode xcb:Allow:SyncPointer)
          button-event window buffer frame fake-last-command)
      (xcb:unmarshal obj data)
      (exwm--log "major-mode=%s buffer=%s"
                 major-mode (buffer-name (current-buffer)))
      (with-slots (detail event state) obj
        (setq button-event (xcb:keysyms:keysym->event exwm--connection
                                                      detail state)
              buffer (exwm--id->buffer event)
              window (get-buffer-window buffer t))
        (cond ((and (eq button-event exwm-input-move-event)
                    buffer
                    ;; Either an undecorated or a floating X window.
                    (with-current-buffer buffer
                      (or (not (derived-mode-p 'exwm-mode))
                          exwm--floating-frame)))
               ;; Move
               (exwm-floating--start-moveresize
                event xcb:ewmh:_NET_WM_MOVERESIZE_MOVE))
              ((and (eq button-event exwm-input-resize-event)
                    buffer
                    (with-current-buffer buffer
                      (or (not (derived-mode-p 'exwm-mode))
                          exwm--floating-frame)))
               ;; Resize
               (exwm-floating--start-moveresize event))
              (buffer
               ;; Click to focus
               (setq fake-last-command t)
               (unless (eq window (selected-window))
                 (setq frame (window-frame window))
                 (unless (eq frame exwm-workspace--current)
                   (if (exwm-workspace--workspace-p frame)
                       ;; The X window is on another workspace
                       (exwm-workspace-switch frame)
                     (with-current-buffer buffer
                       (when (and (derived-mode-p 'exwm-mode)
                                  (not (eq exwm--frame
                                           exwm-workspace--current)))
                         ;; The floating X window is on another workspace
                         (exwm-workspace-switch exwm--frame)))))
                 ;; It has been reported that the `window' may have be deleted
                 (if (window-live-p window)
                     (select-window window)
                   (setq window (get-buffer-window buffer t))
                   (when window (select-window window))))
               ;; Also process keybindings.
               (with-current-buffer buffer
                 (when (derived-mode-p 'exwm-mode)
                   (cl-case exwm--input-mode
                     (line-mode
                      (setq mode (exwm-input--on-ButtonPress-line-mode
                                  buffer button-event)))
                     (char-mode
                      (setq mode (exwm-input--on-ButtonPress-char-mode)))))))
              (t
               ;; Replay this event by default.
               (setq fake-last-command t)
               (setq mode xcb:Allow:ReplayPointer)))
        (when fake-last-command
          (if buffer
              (with-current-buffer buffer
                (exwm-input--fake-last-command))
            (exwm-input--fake-last-command))))
      (xcb:+request exwm--connection
          (make-instance 'xcb:AllowEvents :mode mode :time xcb:Time:CurrentTime))
      (xcb:flush exwm--connection))
    (run-hooks 'exwm-input--event-hook)))

;; https://tbd
;; The call to auth-source-search does not pass any :host and, at
;; least when using only the Secrets API, this returns nil.
(defun my/monkeys--emacs-sql-bug-XXXX nil
  (defun sql-auth-source-search-wallet (wallet product user server database port)
    "Read auth source WALLET to locate the USER secret.
Sets `auth-sources' to WALLET and uses `auth-source-search' to locate the entry.
The DATABASE and SERVER are concatenated with a slash between them as the
host key."
    (let* ((auth-sources wallet)
           host
           secret h-secret sd-secret)

      ;; product
      (setq product (symbol-name product))

      ;; user
      (setq user (unless (string-empty-p user) user))

      ;; port
      (setq port
            (when (and port (numberp port) (not (zerop port)))
              (number-to-string port)))

      ;; server
      (setq server (unless (string-empty-p server) server))

      ;; database
      (setq database (unless (string-empty-p database) database))

      ;; host
      (setq host (if server
                     (if database
                         (concat server "/" database)
                       server)
                   database))

      ;; Perform search
      (dolist (s (auth-source-search :max 1000 :host host))
        (when (and
               ;; Is PRODUCT specified, in the entry, and they are equal
               (if product
                   (if (plist-member s :product)
                       (equal (plist-get s :product) product)
                     t)
                 t)
               ;; Is USER specified, in the entry, and they are equal
               (if user
                   (if (plist-member s :user)
                       (equal (plist-get s :user) user)
                     t)
                 t)
               ;; Is PORT specified, in the entry, and they are equal
               (if port
                   (if (plist-member s :port)
                       (equal (plist-get s :port) port)
                     t)
                 t))
          ;; Is HOST specified, in the entry, and they are equal
          ;; then the H-SECRET list
          (if (and host
                   (plist-member s :host)
                   (equal (plist-get s :host) host))
              (push s h-secret)
            ;; Are SERVER and DATABASE specified, present, and equal
            ;; then the SD-SECRET list
            (if (and server
                     (plist-member s :server)
                     database
                     (plist-member s :database)
                     (equal (plist-get s :server) server)
                     (equal (plist-get s :database) database))
                (push s sd-secret)
              ;; Is SERVER specified, in the entry, and they are equal
              ;; then the base SECRET list
              (if (and server
                       (plist-member s :server)
                       (equal (plist-get s :server) server))
                  (push s secret)
                ;; Is DATABASE specified, in the entry, and they are equal
                ;; then the base SECRET list
                (if (and database
                         (plist-member s :database)
                         (equal (plist-get s :database) database))
                    (push s secret)))))))
      (setq secret (or h-secret sd-secret secret))

      ;; If we found a single secret, return the password
      (when (= 1 (length secret))
        (setq secret (car secret))
        (if (plist-member secret :secret)
            (plist-get secret :secret)
          nil)))))
