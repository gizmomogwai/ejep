;;; ejep-interactive.el --- frontend functions
;;; Commentary:
;;; Code:

(require 'ejep-communication)
(require 'ejep-service)

(defun ejep/content-sync/install-hook ()
  "Install the necessary hooks to sync the content with the jep backend."
  (add-hook 'after-change-functions 'ejep/communication/send-buffer-update nil t))

(defun ejep/disconnect ()
  "Disconnect from a backend process."
  (interactive)
  (ejep/service/rm-process (buffer-file-name)))

(defun ejep/connect ()
  "Connect the current buffer to an backend."
  (interactive)
  (ejep/service/get-process (buffer-file-name))
  (ejep/communication/send-current-buffer)
  (ejep/content-sync/install-hook)
  (add-hook 'kill-buffer-hook 'ejep/disconnect nil t)
  )

(defun ejep/connect-if-possible ()
  "Connect to a jep process if a matching .jep file is found."
  (let* ((found (ejep/service/get-jepconfig-with-pattern-and-command (buffer-file-name))))
    (if found (ejep/connect))))

(add-hook 'find-file-hook 'ejep/connect-if-possible)

(provide 'ejep-interactive)
;;; ejep-interactive ends here
