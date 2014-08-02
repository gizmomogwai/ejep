(defconst ejep/config-filename ".jep"
  "Filename of JEP configuration files.")
(defconst ejep/service/glob-command-regex "\\(.*\\):\n\\(.*\\)\n"
  "Regex used to process .jep files.")
(defconst ejep/protocol/header "^\\([0-9]+\\):\\([0-9]+\\){"
  "Regex to parse jep headers.")

(defconst ejep/service/regex "JEP service, listening on port \\(.*\\)\n"
  "Regex used for finding the communication port of a jep service.")

(defvar ejep/service/process-map (make-hash-table :test 'equal)
  "Maps from config files to created processes. The key is created with `ejep/service/calc-jep-and-pattern-key', the values are a hash with keys :socket and :process to process objects.")

(defvar ejep/problems/buffer "*ejep/problems*" "Buffer for ejep problems.")

(provide 'ejep-variables)

