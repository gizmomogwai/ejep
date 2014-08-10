;;; ejep-variables.el --- defines ejep wide used variables and constants
;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl))

(defconst ejep/config-filename ".jep"
  "Filename of JEP configuration files.")

(defconst ejep/service/glob-command-regex "\\(.*\\):\n\\(.*\\)\n"
  "Regex used to process .jep files.")

(defconst ejep/protocol/header "^\\([0-9]+\\):\\([0-9]+\\){"
  "Regex to parse jep headers.")

(defconst ejep/service/regex "^JEP service, listening on port \\(.*\\)$"
  "Regex used for finding the communication port of a jep service.")

(defvar ejep/service/process-map (make-hash-table :test 'equal)
  "Map from config files to created processes.  The key is created with `ejep/service/calc-jep-and-pattern-key', the values are a hash with keys :socket and :process to process objects.")

(defvar ejep/problems/buffer "*ejep/problems*" "Buffer for ejep problems.")

(defface ejep/problems/faces/fatal
  '((t (:inherit 'button :background "red"))) "ejep problems face for fatal errors" :group 'ejep/problems)

(defface ejep/problems/faces/error
  '((t (:inherit 'button :foreground "red"))) "ejep problems face for errors" :group 'ejep/problems)

(defface ejep/problems/faces/warning
  '((t (:inherit 'button :foreground "yellow"))) "ejep problems face for warnings" :group 'ejep/problems)

(defface ejep/problems/faces/info
  '((t (:inherit 'button :foreground "white"))) "ejep problems face for info" :group 'ejep/problems)

(defface ejep/problems/faces/debug
  '((t (:inherit 'button :foreground "grey"))) "ejep problems face for debug" :group 'ejep/problems)

(defstruct ejep/problems/problem
  "Struct for a problem."
  severity message line column file)

(provide 'ejep-variables)
;;; ejep-variables.el ends here
