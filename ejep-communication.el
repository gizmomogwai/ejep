(eval-when-compile (require 'cl))
(require 'ejep-protocol)

(defun ejep/communication/collect-and-process-output (process content)
  "Gets invoked whenever the server sends data to the client.
It collects the data and checks if enough data for a package of the
JEP-Protocol is received. Then it calls the callback, that is
associated with process."
  (process-put process :ejep/process-data-key (concat (process-get process :ejep/process-data-key) content))
  (let ((json-and-data (ejep/protocol/get-json-and-binary (process-get process :ejep/process-data-key))))
    (if json-and-data
        (progn
          (process-put process :ejep/process-data-key (second json-and-data))
          (funcall (process-get process :ejep/process/callback-key) process (car json-and-data))))))

(defun ejep/communication/json-response-received (process json)
  "called when a complete jep package is received"
  (ejep/protocol/from-server/dispatch json 'ejep/problems/add))


(defun ejep/communication/send-package (package)
  "Send a PACKAGE to the jep backend that is associated with the current buffer."
  (let* ((connection (buffer-local-value 'ejep/communication/connection (current-buffer))))
    (process-send-string connection package)))

(defun ejep/communication/send-current-buffer()
  "Send current buffer content."
  (interactive)
  (let* ((package (ejep/protocol/content-sync-as-string (expand-file-name (buffer-name)) (buffer-string))))
    (ejep/communication/send-package package)))

(defun ejep/communication/send-buffer-update(start end length)
  "Send update on current buffer content."
  (message "send buffer update %s %s %s" start end length)
  (let* ((package (ejep/protocol/content-update-as-string (expand-file-name (buffer-name)) (buffer-string)  (1- start) (1-  end) length)))
    (ejep/communication/send-package package)))

(provide 'ejep-communication)
