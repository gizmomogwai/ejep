;;;; -*- lexical-binding: t -*-
;; ejep.el --- jep implementation in elisp
;; Copyright (C) 2014  Christian Köstlin
;; Author: Christian Köstlin <christian.koestlin@gmail.com>

(require 'ert-expectations)
(require 'el-mock)
(require 'json)
(require 'button)

(provide 'ejep)

(defvar ejep/protocol/header "^\\([0-9]+\\):\\([0-9]+\\){"
  "Regex to parse jep headers.")

(defun ejep/protocol/get-json-data-and-binary (string)
  "Return the unparsed json data and the binary data."
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
  "Like `jep/protocol/get-json-data-and-binary' but the json-data is parsed."
  (let* ((help (ejep/protocol/get-json-data-and-binary string))
         (json (if help (json-read-from-string (car help))))
         (res (if json (list json (second help)))))
    res))

(defun ejep/protocol/create-package (msg binary)
  "converts a elisp message structure and binary data to the desired package format"
  (let* ((json (json-encode msg))
         (json-length (length json))
         (binary-length (length binary))
         (total-length (+ json-length binary-length))
         (res (concat (number-to-string total-length) ":" (number-to-string json-length) json binary)))
    res))

(defun ejep/protocol/content-sync-as-string (absolute-file-name buffer-data)
  (ejep/protocol/create-package (list :_message "ContentSync" :file absolute-file-name) buffer-data))

(defun ejep/protocol/from-server/get-message-type (message)
  "gets the message type from a parsed message or nil"
  (cdr (assoc '_message message)))

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
  (message "%s got %s" process json)
  )

(defun ejep/connect-to-service (port)
  "Connect to a JEP service that is listening on PORT."
  (message "connecting to %S" port)
  (let* ((buffer-name (generate-new-buffer "ejep-connection"))
         (process (open-network-stream "ejep-connection" nil "localhost" port)))
    (set-process-filter-multibyte process t)
    (set-process-coding-system process 'utf-8 'utf-8)
    (process-put process :ejep/process/callback-key 'ejep/communication/json-response-received)
    (set-process-filter process 'ejep/communication/collect-and-process-output)
    ;;    (ACCEPT-process-output process 1 0 t)
    process))

(defun ejep/communication/connect-buffer-to-service()
  "Connects to a jep service and stores the connection buffer local"
  (interactive)
  (set (make-local-variable 'ejep/communication/connection) (ejep/connect-to-service 9001))
  )
(defun ejep/communication/send-current-buffer()
  "Send current buffer content"
  (interactive)
  (let* ((connection (buffer-local-value 'ejep/communication/connection (current-buffer)))
         (package (ejep/protocol/content-sync-as-string (expand-file-name (buffer-name)) (buffer-string))))
         (process-send-string connection package)
         ))

(expectations
  ;; ejep/protocol
  (desc "test jep protocol regexp")
  (expect 0 (string-match ejep/protocol/header "15:1{"))

  (desc "test json protocol stuff")
  (expect nil (string-match ejep/protocol/header "{"))

  (desc "parse well formed jep package with binary data")
  (expect (list "{\"kind\": \"request\"}" "xndjs3") (ejep/protocol/get-json-data-and-binary "25:19{\"kind\": \"request\"}xndjs3trash"))

  (desc "parse well formed jep package without binary data")
  (expect (list "{\"next\":\"message\"}" "") (ejep/protocol/get-json-data-and-binary "18:18{\"next\":\"message\"}"))

  (desc "parse well formed jep package to json and binary")
  (expect '(((kind . "request")) "xndjs3") (ejep/protocol/get-json-and-binary "25:19{\"kind\": \"request\"}xndjs3"))

  (desc "jep content sync message")
  (expect "60:52{\"_message\":\"ContentSync\", \"file\":\"my-file-name.rb\"}the data" (ejep/protocol/content-sync-as-string "my-file-name.rb" "the data"))

  (desc "get message type")
  (expect "ProblemUpdate" (ejep/protocol/from-server/get-message-type (json-read-from-string "{\"fileProblems\":[{\"file\":\"/Users/gizmo/Dropbox/Documents/_projects/jep/ruby-jep/demo/test.rb\",\"problems\":[{\"message\":\"unexpected token $end\",\"line\":16,\"severity\":\"error\"}]}],\"_message\":\"ProblemUpdate\"}")))

  (desc "test dispatch")
  (expect "problem-update-callback"
    (stub ProblemUpdateCallback => "problem-update-callback")
    (ejep/protocol/from-server/dispatch (json-read-from-string "{\"fileProblems\":[{\"file\":\"/Users/gizmo/Dropbox/Documents/_projects/jep/ruby-jep/demo/test.rb\",\"problems\":[{\"message\":\"unexpected token $end\",\"line\":16,\"severity\":\"error\"}]}],\"_message\":\"ProblemUpdate\"}") 'ProblemUpdateCallback))
  ;; ejep/communication

  )
    (ejep/protocol/from-server/dispatch (json-read-from-string "{\"fileProblems\":[{\"file\":\"/Users/gizmo/Dropbox/Documents/_projects/jep/ruby-jep/demo/test.rb\",\"problems\":[{\"message\":\"unexpected token $end\",\"line\":11,\"severity\":\"error\"},{\"message\":\"something different\",\"line\":10,\"severity\":\"warning\"}]}],\"_message\":\"ProblemUpdate\"}") 'ejep/problems/add)
(defun ejep/problems/jump-to (file line)
  "jumps from the problems buffer to the file with the problem"
  (find-file file)
  (goto-line line (find-buffer-visiting file)))

(defun ejep/problems/add (message)
  "add problems to ejeps problems buffer"
  (with-current-buffer (get-buffer-create "*ejep-problems*")
    (erase-buffer)
    (let* ((file-problems (cdr (assoc 'fileProblems message))))
      (mapcar (lambda (file-problem)
                (let* ((file (cdr (assoc 'file file-problem)))
                       (problems (cdr (assoc 'problems file-problem))))
                  (mapcar (lambda (problem)
                            (let* ((severity (cdr (assoc 'severity problem)))
                                   (line (cdr (assoc 'line problem)))
                                   (message (cdr (assoc 'message problem))))
                            (insert-button (format "%s %s:%s - %s\n" severity file line message) 'action (lambda (button) (ejep/problems/jump-to file line))))
                   problems))) file-problems))))

(defun ejep/protocol/from-server/dispatch (message problem-update)
  "dispatches to the right function passing the whole message"
  (let* ((type (ejep/protocol/from-server/get-message-type message)))
    (cond
     ((equal type "ProblemUpdate") (funcall problem-update message))
     )))
;;; (setq jep (ejep/connect-to-service 9001))

;;; ejep.el ends here
;;
;; (insert-button "foo" 'action (lambda (x) (find-file user-init-file)))
;;
