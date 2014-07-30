(require 'ejep-communication)
(require 'ejep-service)

(defun ejep/disconnect ()
    "disconnects from a backend process"
  (interactive)
  (ejep/service/rm-process (buffer-file-name)))

(defun ejep/connect ()
    "connects the current buffer to an backend."
  (interactive)
  (ejep/service/get-process (buffer-file-name) (current-buffer))
  (ejep/communication/send-current-buffer))

(provide 'ejep-interactive)
