(require 'ert-expectations)
(require 'json)

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
  (ejep/protocol/create-package '(:_message "ContentSync" :file absolute-file-name) buffer-data))

(defun ejep/communication/collect-and-process-output (process content)
  "Gets invoked whenever the server sends data to the client.
It collects the data and checks if enough data for a package of the
JEP-Protocol is received. Then it calls the callback, that is
associated with process."
  (process-put process ejep/process-data-key (concat (process-get process ejep/process-data-key) content))
  (let ((json-and-data (ejep/protocol-get-json (process-get process ejep/process-data-key))))
    (if json-and-data
        (progn
          (process-put process ejep/process-data-key (second json-and-data))
          (funcall (process-get process ejep/process/callback-key) process (car json-and-data))))))

(defun ejep/connect-to-service (port)
  "Connect to a JEP service that is listening on PORT."
  (message "connecting to %S" port)
  (let* ((buffer-name (generate-new-buffer "ejep-connection"))
         (process (open-network-stream "ejep-connection" nil "localhost" port)))
    (set-process-filter-multibyte process t)
    (set-process-coding-system process 'utf-8 'utf-8)
    (process-put process ejep/process/callback-key 'ejep/communication/json-response-received)
    (set-process-filter process 'ejep/communication/collect-and-process-output)
    ;;    (ACCEPT-process-output process 1 0 t)
    process))

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
  (expect "62:54{\"_message\":\"ContentSync\",\"file\":\"absolute-file-name\"}the data" (ejep/protocol/content-sync-as-string "/absolute-filename.rb" "the data"))

  ;; ejep/communication

  )

                                        ;(setq jep (ejep/connect-to-service 9001))
