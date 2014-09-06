;;; ejep-service.el --- handles communication with the jep process
;;; Commentary:
;;; Code:

(require 'ejep-communication)

(defun ejep/service/extract-first-and-second-from-match (text)
  "Return list with the first and second match-group of the last search on TEXT.
See `string-match' and `match-string'."
  (list (match-string 1 text) (match-string 2 text)))

(defun ejep/service/parent-directory (directory)
  "Return the parent directory of DIRECTORY."
  (file-name-directory (directory-file-name directory)))

(defun ejep/service/find-matching-jep-file
    (filename exists-p matches-p &optional path)
  "Return the best matching jep-file, command and pattern.
The first matching file is an existing .jep file in FILENAME's
directory hierarchie, that has a command associated with FILENAME.
EXISTS-P and MATCHES-P are used to analyze this.  EXISTS-P is usually
just `file-exists-p', MATCHES has to open the file and check if it
contains a matching pattern and return the associated command.  PATH
is used for recursion, so it can be nil for the first call, but is
then internally used with the path-hierarchy."
  (let* ((current-dir (expand-file-name (if path path (file-name-directory filename))))
         (jep-file-name (expand-file-name ejep/config-filename current-dir))
         (jep-exists (funcall exists-p jep-file-name))
         (finished (and jep-exists (funcall matches-p jep-file-name filename))))
    (if finished (list jep-file-name (first finished) (second finished))
      (let* ((parent-dir (ejep/service/parent-directory current-dir))
             (new-dir (not (string= parent-dir current-dir))))
        (if new-dir (ejep/service/find-matching-jep-file filename exists-p matches-p parent-dir))))))

;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman
;; <zzbba...@aol.com>”. 2010-09-02
(defun ejep/service/get-string-from-file (filename)
  "Return FILENAME's file content."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun ejep/service/glob-pattern-to-regexp (pattern)
  "Return regexp matching on the given PATTERN."
  (let ((res ""))
    (mapc (lambda (c) (cond
                       ((char-equal c ?.) (setq res (concat res (list ?\\ ?.))))
                       ((char-equal c ?*) (setq res (concat res (list ?. ?*))))
                       (t (setq res (concat res (list c)))))) pattern)
    res))

(defun ejep/service/string-match-fully-p (pattern text)
  "Return t if PATTERN match TEXT fully."
  (let ((match (string-match pattern text)))
    (if match (eq (match-end 0) (length text)) nil)))

(defun ejep/service/glob-pattern-command-matcher (pairs text)
  "Return the first pair of PAIRS whose regexp is a match for TEXT.
PAIRS is a list of regexp strings and commands."
  (dolist (head pairs)
    (if (ejep/service/string-match-fully-p (ejep/service/glob-pattern-to-regexp (car head)) text)
        (return head))))

(defun ejep/service/glob-pattern-command-matcher-with-file (pattern-command-file filename)
  "Return a matching command from PATTERN-COMMAND-FILE for FILENAME or nil."
  (let* ((pairs (ejep/service/map-regex-with-file
                 ejep/service/glob-command-regex ejep/config-filename  (function ejep/service/extract-first-and-second-from-match))))
    (ejep/service/glob-pattern-command-matcher pairs filename)))

(defun ejep/service/map-regex (regex text fn)
  "Map the REGEX over the TEXT executing FN.
FN is called for each successful `string-match' for the content of FILENAME.
Returns the results of the FN as a list."
  (let* ((search-idx 0)
         (res))
    (while (string-match regex text search-idx)
      (setq res (append res (list (funcall fn text))))
      (setq search-idx (match-end 0)))
    res))

(defun ejep/service/map-regex-with-file (regex filename fn)
  "Map the REGEX over the content of FILENAME executing FN."
  (ejep/service/map-regex regex (ejep/service/get-string-from-file
                                 filename) fn))

(defun ejep/service/get-jepconfig-with-pattern-and-command (filename)
  "Return jep-config, pattern and command for FILENAME or nil."
  (ejep/service/find-matching-jep-file filename 'file-exists-p 'ejep/service/glob-pattern-command-matcher-with-file))


(defun ejep/service/calc-jep-and-pattern-key (jep pattern)
  "creates an hash lookup key"
  (format "%s@%s" pattern jep))

(defun ejep/service/start-backend-process (command filename)
  "Execute COMMAND to create the jep process for FILENAME.
Return the created process together with the tcp port used for communications."
  (let* ((process-connection-type nil)
         (buffer-name (generate-new-buffer (format "*jep-process-for--%s*" filename)))
         (interpolated-command (substitute-in-file-name command))
         (process (apply 'start-process "jep-process" buffer-name (split-string interpolated-command))))
    (accept-process-output process 1 nil t)
    (list process
          (let* ((string (with-current-buffer buffer-name (buffer-string)))
                 (bummer (string-match ejep/service/regex string))
                 (match (match-string 1 string))
                 (res (if match (string-to-number match) nil)))
            res))))

(defun ejep/service/connect (port filename)
  "Connect to a jep service that is listening on PORT and that is responsible for FILENAME."
  (message "connecting to %S" port)
  (let* ((name (format "*ejep-connection-for--%s" filename))
         (process (open-network-stream name nil "localhost" port)))
    ;;;(set-process-filter-multibyte process t)
    ;;;(set-process-coding-system process 'utf-8 'utf-8)
    (process-put process :ejep/process/callback-key 'ejep/communication/json-response-received)
    (set-process-filter process 'ejep/communication/collect-and-process-output)
;;    (ACCEPT-process-output process 1 0 t)
    process))

(defun ejep/service/connect-to-backend-process (command filename)
  "Launch the defined COMMAND for FILENAME and return the background process together with the socket to this backend."
  (let* ((process-and-port (ejep/service/start-backend-process command filename))
         (process (first process-and-port))
         (port (second process-and-port)))
    (if port
        (list
         process
         (ejep/service/connect port filename)))))

(defun ejep/service/rm-process (filename)
  "remove the backend process from the system for a FILENAME."
  (let* ((found (ejep/service/get-jepconfig-with-pattern-and-command filename))
         (jepconfig (first found))
         (pattern (second found))
         (command (third found))
         (key (ejep/service/calc-jep-and-pattern-key jepconfig pattern))
         (already-launched (gethash key ejep/service/process-map)))
    (if already-launched
        (let* ((socket (gethash :socket already-launched))
               (process (gethash :process already-launched)))
          (delete-process process)
          (remhash key ejep/service/process-map)))))

(defun ejep/service/get-or-create-process (filename)
  "Return the backend process responsible for FILENAME."
  (let* ((found (ejep/service/get-jepconfig-with-pattern-and-command filename))
         (jepconfig (first found))
         (pattern (second found))
         (command (third found))
         (key (ejep/service/calc-jep-and-pattern-key jepconfig pattern))
         (already-launched (gethash key ejep/service/process-map)))
    (if already-launched
        (gethash :socket already-launched)
      (let* ((launch-info (ejep/service/connect-to-backend-process command filename))
             (process (first launch-info))
             (socket (second launch-info)))
        (if (and process socket)
            (let* ((res (make-hash-table :test 'equal))
                   (new-entry (make-hash-table :test 'equal)))
              (puthash :process process new-entry)
              (puthash :socket socket new-entry)
              (puthash key new-entry ejep/service/process-map)
              (set (make-local-variable 'ejep/communication/connection) socket)
              socket)
          nil)))))

(provide 'ejep-service)
;;; ejep-service.el ends here

