;;; ejep-problems.el --- problem list for ejep
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(require 'ejep-variables)
(require 'dash)
(defun ejep/problems/face-for-severity (severity)
  "Return a face for SEVERITY."
  (format "ejep/problems/faces/%s" severity))

(defun ejep/problems/format (text font)
  "Format TEXT with FONT."
  (propertize text 'font-lock-face font))

(defun ejep/problems/number-to-string (number-or-nil)
  "Convert NUMBER-OR-NIL to a string."
  (if number-or-nil (number-to-string number-or-nil) ""))

(defun ejep/problems/add-a-problem-for-a-file (file problem)
  "For one FILE add one PROBLEM to the ejep problems buffer."
  (let* ((severity (cdr (assoc 'severity problem)))
         (face (ejep/problems/face-for-severity severity))
         (severity-string (ejep/problems/format (substring severity 0 1) face))
         (message (cdr (assoc 'message problem)))
         (line (cdr (assoc 'line problem)))
         (line-string (ejep/problems/number-to-string line))
         (column (cdr (assoc 'column problem)))
         (column-string (ejep/problems/number-to-string column))
         (problem (make-ejep/problems/problem
                   :severity severity-string
                   :message message
                   :line line-string
                   :column column-string
                   :file file)))
    (add-to-list
     'tabulated-list-entries
     (list problem
           (vector severity-string
                   (list message
                         'follow-link t
                         'ejep/problems/attachment problem
                         'action 'ejep/problems/goto
                         'face face)
                   line-string
                   column-string
                   file)))))

(defun ejep/problems/add-problems-for-a-file(file-problems)
  "Adds all FILE-PROBLEMS to ejeps problems buffer for a file."
  (let* ((file (cdr (assoc 'file file-problems)))
         (problems (cdr (assoc 'problems file-problems))))
    (mapcar (-partial 'ejep/problems/add-a-problem-for-a-file file) problems)))

(defun ejep/problems/get-buffer()
  "Return the ejep problems buffer in the correct mode, no matter what."
  (let* ((buffer (get-buffer-create ejep/problems/buffer)))
    (with-current-buffer buffer
      (ejep/problems/mode))
    buffer))

(defun ejep/problems/add(message)
  "Add all problems from a MESSAGE to ejeps problems buffer."
  (with-current-buffer (ejep/problems/get-buffer)
    (set 'tabulated-list-entries nil)
    (let* ((file-problems (cdr (assoc 'fileProblems message))))
      (mapc 'ejep/problems/add-problems-for-a-file file-problems))
    (tabulated-list-revert)))

(define-derived-mode ejep/problems/mode tabulated-list-mode "Jep problems"
  "Major mode for listing jep problems."
  (setq tabulated-list-format [("S" 1 t :right-align t)
                               ("Message" 40 t)
                               ("Line" 4 t :right-align t)
                               ("Col" 3 t :right-align t)
                               ("File" 0 t)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun ejep/problems/list()
  "Shows the ejep problems list."
  (interactive)
  (unless (get-buffer ejep/problems/buffer)
    (with-current-buffer (get-buffer-create ejep/problems/buffer)
      (ejep/problems/mode)))
  (display-buffer ejep/problems/buffer))

(defun ejep/problems/goto-lowlevel (line target-buffer)
  "Low-level function that should jump to a LINE in a TARGET-BUFFER."
  (pop-to-buffer target-buffer 'other-window)
  (with-current-buffer target-buffer
    (goto-char (point-min))
    (forward-line (1- (string-to-number line)))))

(defun ejep/problems/goto (button)
  "Called from a BUTTON entry in the ejep problems-table."
  (let* ((problem (button-get button 'ejep/problems/attachment))
         (file (ejep/problems/problem-file problem))
         (line (ejep/problems/problem-line problem))
         (target-buffer (or (find-buffer-visiting file) (find-file file))))
    (ejep/problems/goto-lowlevel line target-buffer)))

(provide 'ejep-problems)
;;; ejep-problems.el ends here

