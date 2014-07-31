(require 'ejep-communication)
(require 'ejep-service)

(defun ejep/content-sync/install-hook ()
  "Installs the necessary hooks to sync the content with the jep backend."
  (add-hook 'after-change-functions 'ejep/communication/send-buffer-update nil t))

(defun ejep/disconnect ()
    "disconnects from a backend process."
  (interactive)
  (ejep/service/rm-process (buffer-file-name)))

(defun ejep/connect ()
    "connects the current buffer to an backend."
  (interactive)
  (ejep/service/get-process (buffer-file-name) (current-buffer))
  (ejep/communication/send-current-buffer)
  (ejep/content-sync/install-hook))

(provide 'ejep-interactive)
