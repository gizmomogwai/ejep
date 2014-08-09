;;; ejep-protocol.el --- protocol layer of ejep
;;; Commentary:
;; packages and unpackages jep message

;;; Code:

(require 'ejep-variables)
(require 'json)

(defun ejep/protocol/get-json-data-and-binary (string)
  "Dissect the STRING of the message into json data and binary data."
  (let* ((match (string-match ejep/protocol/header string))
         (overall-length (if match (string-to-number (match-string 1 string))))
         (json-length (if match (string-to-number (match-string 2 string))))
         (header-length (if match (match-end 2)))
         (json-end (+ header-length json-length))
         (binary-end (+ json-end (- overall-length json-length))))
    (if (>= (length string) overall-length)
        (list
         (substring string header-length json-end)
         (substring string json-end binary-end))
      nil)))

(defun ejep/protocol/get-json-and-binary (string)
  "Process the STRING like `jep/protocol/get-json-data-and-binary', but parse the json-data."
  (let* ((help (ejep/protocol/get-json-data-and-binary string))
         (json (if help (json-read-from-string (car help))))
         (res (if json (list json (second help)))))
    res))

(defun ejep/protocol/create-package (msg binary)
  "Convert a elisp MSG structure and BINARY data to the desired package format."
  (let* ((json (json-encode msg))
         (json-length (length json))
         (binary-length (length binary))
         (total-length (+ json-length binary-length))
         (res (concat (number-to-string total-length) ":" (number-to-string json-length) json binary)))
    res))

(defconst ejep/protocol/backend-shutdown "Shutdown" "Message type for shutdown messages.")
(defconst ejep/protocol/content-sync "ContentSync" "Message type for content-sync messages.")
(defconst ejep/protocol/problem-update "ProblemUpdate" "Message type for problem update messages.")
(defun ejep/protocol/backend-shutdown-as-string ()
  "Create the string representation of a Backend Shutdown message."
  (ejep/protocol/create-package (list :_message ejep/protocol/backend-shutdown) nil))

(defun ejep/protocol/content-sync-as-string (absolute-file-name buffer-data)
  (ejep/protocol/create-package (list :_message ejep/protocol/content-sync :file absolute-file-name) buffer-data))

(defun ejep/protocol/content-update-as-string (absolute-file-name buffer-data start end length)
  "Create the json object given the ABSOLUTE-FILE-NAME the BUFFER-DATA and the START, END and LENGTH hook parameters."
  (let* ((diff (- end start))
         (delete-operation (> length diff))
         (first-index start)
         (end-index (+ first-index length))
         (data (if delete-operation "" (substring buffer-data start end))))
    (ejep/protocol/create-package (list :_message ejep/protocol/content-sync :file absolute-file-name :start first-index :end end-index) data)))

(defun ejep/protocol/from-server/get-message-type (message)
  "Gets the type from a parsed MESSAGE, or nil."
  (cdr (assoc '_message message)))

(defun ejep/protocol/from-server/dispatch (message problem-update)
  "Dispatch the MESSAGE to the right function for the PROBLEM-UPDATE."
  (let* ((type (ejep/protocol/from-server/get-message-type message)))
    (cond
     ((equal type ejep/protocol/problem-update) (funcall problem-update message)))))

(provide 'ejep-protocol)
;;; ejep-protocol.el ends here
