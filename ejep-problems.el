;; -*- lexical-binding: t -*-
(defun ejep/problems/jump-to (file line)
  "jumps from the problems buffer to the FILE with the problem in LINE."
  (find-file file)
  (let* ((current-prefix-arg line)
         (buffer (find-buffer-visiting file)))
    (call-interactively 'goto-line buffer)))

(defface ejep/problems/faces/F
  '((t (:inherit 'button :background "red"))) "ejep problems face for fatal errors" :group 'ejep/problems)
(defface ejep/problems/faces/E
  '((t (:inherit 'button :foreground "red"))) "ejep problems face for errors" :group 'ejep/problems)
(defface ejep/problems/faces/W
  '((t (:inherit 'button :foreground "yellow"))) "ejep problems face for warnings" :group 'ejep/problems)
(defface ejep/problems/faces/I
  '((t (:inherit 'button :foreground "white"))) "ejep problems face for info" :group 'ejep/problems)
(defface ejep/problems/faces/D
  '((t (:inherit 'button :foreground "grey"))) "ejep problems face for debug" :group 'ejep/problems)

(defun ejep/problems/face-for-severity (severity)
  "returns a face for severity"
  (format "ejep/problems/faces/%s" severity))

(defun ejep/problems/severity-to-code (severity)
  "Returns a severity code."
  (substring severity 0 1))
(defstruct ejep/problems/problem
  "Struct for a problem."
  severity message line column file)
(make-ejep/problems/problem :severity 123)
;;(insert-button "test" 'face "ejep/problems/faces/fatal")test
;;(insert-button "test" :face (ejep/problems/face-for-severity "fatal"))test
(defun ejep/problems/add-a-problem-for-a-file (file problem)
  "Adds one PROBLEM for a FILE to the ejep problems buffer."
  (let* ((severity (ejep/problems/severity-to-code (cdr (assoc 'severity problem))))
         (message (cdr (assoc 'message problem)))
         (line (cdr (assoc 'line problem)))
         (line-string (if line (number-to-string line) ""))
         (column (cdr (assoc 'column problem)))
         (column-string (if column (number-to-string column) ""))
         (problem (make-ejep/problems/problem
                   :severity severity
                   :message message
                   :line line-string
                   :column column-string
                   :file file)))
    (add-to-list
     'tabulated-list-entries
     (list problem
           (vector severity
                 (list message 'follow-link t 'ejep/problems/attachment problem 'action 'ejep/problems/goto)
                 line-string
                 column-string
                 file)))))


  (defun ejep/problems/add-problems-for-a-file (file-problems)
    "Adds all FILE-PROBLEMS to ejeps problems buffer for a file."
    (let* ((file (cdr (assoc 'file file-problems)))
           (problems (cdr (assoc 'problems file-problems))))
      (mapcar (-partial 'ejep/problems/add-a-problem-for-a-file file) problems)))

  (defun ejep/problems/add (message)
    "add problems to ejeps problems buffer"
    (with-current-buffer (get-buffer-create ejep/problems/buffer)
      (set 'tabulated-list-entries nil)
      (let* ((file-problems (cdr (assoc 'fileProblems message))))
        (mapcar 'ejep/problems/add-problems-for-a-file file-problems))
      (tabulated-list-revert)))
;;(let* ((files-problems (cdr (assoc 'fileProblems message))))
;;(mapcar 'ejep/problems/add-for-file files-problems))))



(define-derived-mode ejep/problems/mode tabulated-list-mode "Jep problems"
  "Major mode for listing jep problems."
  (setq tabulated-list-format [("S" 1 t :right-align t)
                               ("Message" 80 t)
                               ("Line" 4 t :right-align t)
                               ("Col" 3 t :right-align t)
                               ("File" 0 t)])
  (setq tabulated-list-padding 1)
  ;; tabulated-list-entries #'flycheck-error-list-entries
  ;(add-hook 'tabulated-list-revert-hook #'flycheck-error-list-set-mode-line
   ;         nil 'local)
  (tabulated-list-init-header))


(defun ejep/problems/list ()
  (interactive)
  (unless (get-buffer ejep/problems/buffer)
    (with-current-buffer (get-buffer-create ejep/problems/buffer)
      (ejep/problems/mode)))
  (display-buffer ejep/problems/buffer))

(defun ejep/problems/goto-lowlevel (line target-buffer)
  (pop-to-buffer target-buffer 'other-window)
  (with-current-buffer target-buffer
    (goto-char (point-min))
    (forward-line (1- (string-to-number line)))))

(defun ejep/problems/goto (button)
  (let* ((problem (button-get button 'ejep/problems/attachment))
         (file (ejep/problems/problem-file problem))
         (line (ejep/problems/problem-line problem))
         (target-buffer (find-buffer-visiting file))
         (target-buffer-life (buffer-live-p target-buffer)))
    (ejep/problems/goto-lowlevel line target-buffer)))

(provide 'ejep-problems)
