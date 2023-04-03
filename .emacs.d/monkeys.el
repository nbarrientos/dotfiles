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
