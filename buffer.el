(defun vertico--display-in-buffer (candidates)
  (let ((buffer (get-buffer-create (format " *vertico-%s*" (- (recursion-depth) 1)))))
    (with-current-buffer buffer
      (setq-local display-line-numbers nil
                  show-trailing-whitespace nil
                  inhibit-modification-hooks t)
      (erase-buffer)
      (insert (string-join candidates))
      (goto-char (point-min)))
    (display-buffer buffer
                    `(display-buffer-in-side-window
                      (window-parameters (mode-line-format . none))
                      (window-height . ,vertico-count)
                      (side . bottom)
                      (slot . -1)))))

(advice-add #'vertico--display-candidates
            :override #'vertico--display-in-buffer)
