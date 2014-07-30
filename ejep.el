;; -*- lexical-binding: t -*-
;; ejep.el --- jep implementation in elisp
;; Copyright (C) 2014  Christian Köstlin
;; Author: Christian Köstlin <christian.koestlin@gmail.com>

(require 'ert-expectations)
(require 'el-mock)
(require 'json)
(require 'button)

(provide 'ejep)

(defvar ejep/config-filename ".jep"
  "Filename of JEP configuration files.")
(defvar ejep/servicestartup/glob-command-regex "\\(.*\\):\n\\(.*\\)\n"
  "Regex used to process .jep files.")
(defvar ejep/protocol/header "^\\([0-9]+\\):\\([0-9]+\\){"
  "Regex to parse jep headers.")

;;;"JEP service, listening on port <port>\n"

(defun ejep/servicestartup/extract-first-and-second-from-match (text)
  "Return list with the first and second match-group.
See `string-match' and `match-string'."
  (list (match-string 1 text) (match-string 2 text)))

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
  (ejep/protocol/from-server/dispatch json 'ejep/problems/add)
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

(defun ejep/problems/jump-to (file line)
  "jumps from the problems buffer to the file with the problem"
  (find-file file)
  (goto-line line (find-buffer-visiting file)))

(defface ejep/problems/faces/fatal '((t (:inherit 'button :background "red"))) "ejep problems face for fatal errors" :group 'ejep/problems)
(defface ejep/problems/faces/error '((t (:inherit 'button :foreground "red"))) "ejep problems face for errors" :group 'ejep/problems)
(defface ejep/problems/faces/warn '((t (:inherit 'button :foreground "yellow"))) "ejep problems face for warnings" :group 'ejep/problems)
(defface ejep/problems/faces/info '((t (:inherit 'button :foreground "white"))) "ejep problems face for info" :group 'ejep/problems)
(defface ejep/problems/faces/debug '((t (:inherit 'button :foreground "grey"))) "ejep problems face for debug" :group 'ejep/problems)
(defun ejep/problems/face-for-severity (severity)
  "returns a face for severity"
  (format "ejep/problems/faces/%s" severity))

;;(insert-button "test" 'face "ejep/problems/faces/fatal")test
;;(insert-button "test" :face (ejep/problems/face-for-severity "fatal"))test

(defun ejep/problems/add-problem-for-file (file problem)
  "adds one problem for a file to the ejep problems buffer"
  (let* ((severity (cdr (assoc 'severity problem)))
         (line (cdr (assoc 'line problem)))
         (message (cdr (assoc 'message problem))))
    (insert-button (format "%s %s:%s - %s\n" severity file line message)
                   'action (lambda (button) (ejep/problems/jump-to file line))
                   'face (ejep/problems/face-for-severity severity))))

(defun ejep/problems/add-for-file (file-problems)
  "adds problems to ejeps problems buffer for one file"
  (let* ((file (cdr (assoc 'file file-problems)))
         (problems (cdr (assoc 'problems file-problems))))
    (mapcar (-partial 'ejep/problems/add-problem-for-file file) problems)))

(defun ejep/problems/add (message)
  "add problems to ejeps problems buffer"
  (with-current-buffer (get-buffer-create "*ejep-problems*")
    (erase-buffer)
    (let* ((files-problems (cdr (assoc 'fileProblems message))))
      (mapcar 'ejep/problems/add-for-file files-problems))))

(defun ejep/protocol/from-server/dispatch (message problem-update)
  "dispatches to the right function passing the whole message"
  (let* ((type (ejep/protocol/from-server/get-message-type message)))
    (cond
     ((equal type "ProblemUpdate") (funcall problem-update message))
     )))

(defun ejep/servicestartup/parent-directory(directory)
  "Return the parent directory of DIRECTORY."
  (file-name-directory (directory-file-name directory)))

(defun ejep/servicestartup/find-matching-jep-file
  (filename exists-p matches-p &optional path)
  "Return the jep-file, the command and the pattern from the first matching .jep file for FILENAME or nil.
The first matching file is an existing .jep file in FILENAME's directory hierarchie, that has a command associated with FILENAME. EXISTS-P and MATCHES-P are used to analyze this. EXISTS-P is usually just `file-exists-p', MATCHES has to open the file and check if it contains a matching pattern and return the associated command."
  (let* ((current-dir (expand-file-name (if path path (file-name-directory filename))))
         (jep-file-name (expand-file-name ejep/config-filename current-dir))
         (jep-exists (funcall exists-p jep-file-name))
         (finished (and jep-exists (funcall matches-p jep-file-name filename))))
    (if finished (list jep-file-name (first finished) (second finished))
      (let* ((parent-dir (ejep/servicestartup/parent-directory current-dir))
             (new-dir (not (string= parent-dir current-dir))))
        (if new-dir (ejep/servicestartup/find-matching-jep-file filename exists-p matches-p parent-dir))))))

;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman <zzbba...@aol.com>”. 2010-09-02
(defun ejep/servicestartup/get-string-from-file(filename)
  "Return FILENAME's file content."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun ejep/servicestartup/glob-pattern-to-regexp(pattern)
  "Return regexp matching on the given PATTERN."
  (let ((res ""))
    (mapc (lambda (c) (cond
                       ((char-equal c ?.) (setq res (concat res (list ?\\ ?.))))
                       ((char-equal c ?*) (setq res (concat res (list ?. ?*))))
                       (t (setq res (concat res (list c)))))) pattern)
    res))

(defun ejep/servicestartup/map-regex (text regex fn)
  "Map the REGEX over the FILENAME executing FN.
   FN is called for each successful `string-match' for the content of FILENAME.
   Returns the results of the FN as a list."
  (let* ((search-idx 0)
         (res))
    (while (string-match regex text search-idx)
      (setq res (append res (list (funcall fn text))))
      (setq search-idx (match-end 0)))
    res))

(defun ejep/servicestartup/string-match-fully-p (pattern text)
  "Return t if PATTERN matches TEXT fully."
  (let ((match (string-match pattern text)))
    (if match (eq (match-end 0) (length text)) nil)))

(defun ejep/servicestartup/glob-pattern-command-matcher (pairs text)
  "Return the matching pattern and command for TEXT.
PAIRS is a list of regexp strings and commands."
  (dolist (head pairs)
    (if (ejep/servicestartup/string-match-fully-p (ejep/servicestartup/glob-pattern-to-regexp (car head)) text)
        (return head))))

(defun ejep/servicestartup/glob-pattern-command-matcher-with-file (pattern-command-file filename)
  "Return a matching command from PATTERN-COMMAND-FILE for FILENAME or nil."
  (let* ((pairs (ejep/servicestartup/map-regex-with-file ejep/config-filename ejep/servicestartup/glob-command-regex (function ejep/servicestartup/extract-first-and-second-from-match))))
    (ejep/servicestartup/glob-pattern-command-matcher pairs filename)))

(defun ejep/servicestartup/map-regex-with-file (filename regex fn)
  "Use `ejep/servicestartup/map-regex' with the contents of FILENAME."
  (ejep/servicestartup/map-regex (ejep/servicestartup/get-string-from-file filename) regex fn))

(defun ejep/servicestartup/get-jepconfig-with-pattern-and-command (filename)
  "Return jep-config, pattern and command for FILENAME or nil."
  (ejep/servicestartup/find-matching-jep-file filename 'file-exists-p 'ejep/servicestartup/glob-pattern-command-matcher-with-file))

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
  (expect "59:51{\"_message\":\"ContentSync\",\"file\":\"my-file-name.rb\"}the data" (ejep/protocol/content-sync-as-string "my-file-name.rb" "the data"))

  (desc "get message type")
  (expect "ProblemUpdate" (ejep/protocol/from-server/get-message-type (json-read-from-string "{\"fileProblems\":[{\"file\":\"/Users/gizmo/Dropbox/Documents/_projects/jep/ruby-jep/demo/test.rb\",\"problems\":[{\"message\":\"unexpected token $end\",\"line\":16,\"severity\":\"error\"}]}],\"_message\":\"ProblemUpdate\"}")))

  (desc "test dispatch")
  (expect "problem-update-callback"
    (stub ProblemUpdateCallback => "problem-update-callback")
    (ejep/protocol/from-server/dispatch (json-read-from-string "{\"fileProblems\":[{\"file\":\"/Users/gizmo/Dropbox/Documents/_projects/jep/ruby-jep/demo/test.rb\",\"problems\":[{\"message\":\"unexpected token $end\",\"line\":16,\"severity\":\"error\"}]}],\"_message\":\"ProblemUpdate\"}") 'ProblemUpdateCallback))
  ;; ejep/communication

  (desc "directory-file-name gets the filename of a directory")
  (expect "/abc/def" (directory-file-name "/abc/def/"))

  (desc "file-name-directory gets the parent directory for a non directory file")
  (expect "/abc/" (file-name-directory "/abc/test.txt"))

  (desc "file-name-directory gets itself for a directory")
  (expect "/abc/test/" (file-name-directory "/abc/test/"))

  (desc "ejep/servicestartup/parent-directory")
  (expect "/abc/" (ejep/servicestartup/parent-directory "/abc/def/"))

  (desc "ejep/servicestartup/parent-directory for root yields root")
  (expect "/" (ejep/servicestartup/parent-directory "/"))

  (desc "ejep/servicestartup/find-matching-jep-file on same level as current file")
  (expect
      (list (expand-file-name "/my/very/long/path/.jep") "*.test" "command")
    (ejep/servicestartup/find-matching-jep-file "/my/very/long/path/123.txt"
                                                (lambda (file-name) (string= (expand-file-name "/my/very/long/path/.jep") file-name))
                                                (lambda (config filename) '("*.test" "command"))))

  (desc "ejep/servicestartup/find-matching-jep-file on level above current file")
  (expect
      (list (expand-file-name "/my/very/long/.jep") "*.test" "command")
    (ejep/servicestartup/find-matching-jep-file "/my/very/long/path/123.txt"
                                                (lambda (file-name) (string= (expand-file-name "/my/very/long/.jep") file-name))
                                                (lambda (config filename) '("*.test" "command"))))

  (desc "transform glob-pattern to emacs regexp")
  (expect "abc" (ejep/servicestartup/glob-pattern-to-regexp "abc"))

  (desc "transform glob-pattern with . to emacs regexp")
  (expect "a\\.b" (ejep/servicestartup/glob-pattern-to-regexp "a.b"))

  (desc "transform glob-pattern with * to emacs regexp")
  (expect "a.*b" (ejep/servicestartup/glob-pattern-to-regexp "a*b"))

  (desc "ejep/servicestartup/map-regex")
  (expect '(("abc" "def") ("ghi" "jkl")) (ejep/servicestartup/map-regex "abc:\ndef\nghi:\njkl\n" ejep/servicestartup/glob-command-regex 'ejep/servicestartup/extract-first-and-second-from-match))

  (desc "ejep/servicestartup/map-regex-with-file")
  (expect '(("*.test1" "command1") ("*.test2" "command2"))
    (ejep/servicestartup/map-regex-with-file ".jep" ejep/servicestartup/glob-command-regex 'ejep/servicestartup/extract-first-and-second-from-match))

  (desc "string-match-fully-p matches")
  (expect t
    (ejep/servicestartup/string-match-fully-p "abc" "abc"))

  (desc "string-match-fully-p matches not")
  (expect nil
    (ejep/servicestartup/string-match-fully-p "abc" "abcd"))

  (desc "glob-pattern-command-matcher")
  (expect '("*.text" "command1")
    (ejep/servicestartup/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text"))

  (desc "glob-pattern-command-matcher")
  (expect '("*.text2" "command2")
    (ejep/servicestartup/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text2"))

  (desc "glob-pattern-command-matcher")
  (expect nil
    (ejep/servicestartup/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text3"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect '("*.test1" "command1")
    (ejep/servicestartup/glob-pattern-command-matcher-with-file "./.jep" "test.test1"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect '("*.test2" "command2")
    (ejep/servicestartup/glob-pattern-command-matcher-with-file "./.jep" "test.test2"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect nil
    (ejep/servicestartup/glob-pattern-command-matcher-with-file "./.jep" "test.test3"))

  (desc "convinient get jepconfig with pattern and command")
  (expect (list (expand-file-name "./.jep") "*.test2" "command2") (ejep/servicestartup/get-jepconfig-with-pattern-and-command "./blub.test2"))

  (desc "convinient get jepconfig with pattern and command -> nil")
  (expect nil (ejep/servicestartup/get-jepconfig-with-pattern-and-command "./blub.test3"))

)

;;; (setq jep (ejep/connect-to-service 9001))
;;; ejep.el ends here
