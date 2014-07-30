(require 'f)

(defvar ejep/test-path
  (f-dirname (f-this-file)))

(defvar ejep/root-path
  (f-parent ejep/test-path))

(add-to-list 'load-path ejep/root-path)
