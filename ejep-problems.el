;; -*- lexical-binding: t -*-
(defun ejep/problems/jump-to (file line)
  "jumps from the problems buffer to the FILE with the problem in LINE."
  (find-file file)
  (let* ((current-prefix-arg line)
         (buffer (find-buffer-visiting file)))
    (call-interactively 'goto-line buffer)))

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

(provide 'ejep-problems)
