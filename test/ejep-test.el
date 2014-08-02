(require 'ert-expectations)
(require 'el-mock)
(require 'ejep)

(expectations
  ;; protocol
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

  ;; stdapi
  (desc "directory-file-name gets the filename of a directory")
  (expect "/abc/def" (directory-file-name "/abc/def/"))

  (desc "file-name-directory gets the parent directory for a non directory file")
  (expect "/abc/" (file-name-directory "/abc/test.txt"))

  (desc "file-name-directory gets itself for a directory")
  (expect "/abc/test/" (file-name-directory "/abc/test/"))

  ;; service
  (desc "ejep/service/parent-directory")
  (expect "/abc/" (ejep/service/parent-directory "/abc/def/"))

  (desc "ejep/service/parent-directory for root yields root")
  (expect "/" (ejep/service/parent-directory "/"))

  (desc "ejep/service/find-matching-jep-file on same level as current file")
  (expect
      (list (expand-file-name "/my/very/long/path/.jep") "*.test" "command")
    (ejep/service/find-matching-jep-file "/my/very/long/path/123.txt"
                                                (lambda (file-name) (string= (expand-file-name "/my/very/long/path/.jep") file-name))
                                                (lambda (config filename) '("*.test" "command"))))

  (desc "ejep/service/find-matching-jep-file on level above current file")
  (expect
      (list (expand-file-name "/my/very/long/.jep") "*.test" "command")
    (ejep/service/find-matching-jep-file "/my/very/long/path/123.txt"
                                                (lambda (file-name) (string= (expand-file-name "/my/very/long/.jep") file-name))
                                                (lambda (config filename) '("*.test" "command"))))

  (desc "transform glob-pattern to emacs regexp")
  (expect "abc" (ejep/service/glob-pattern-to-regexp "abc"))

  (desc "transform glob-pattern with . to emacs regexp")
  (expect "a\\.b" (ejep/service/glob-pattern-to-regexp "a.b"))

  (desc "transform glob-pattern with * to emacs regexp")
  (expect "a.*b" (ejep/service/glob-pattern-to-regexp "a*b"))

  (desc "ejep/service/map-regex")
  (expect '(("abc" "def") ("ghi" "jkl")) (ejep/service/map-regex "abc:\ndef\nghi:\njkl\n" ejep/service/glob-command-regex 'ejep/service/extract-first-and-second-from-match))

  (desc "ejep/service/map-regex-with-file")
  (expect '(("*.test1" "command1") ("*.test2" "command2"))
    (ejep/service/map-regex-with-file ".jep" ejep/service/glob-command-regex 'ejep/service/extract-first-and-second-from-match))

  (desc "string-match-fully-p matches")
  (expect t
    (ejep/service/string-match-fully-p "abc" "abc"))

  (desc "string-match-fully-p matches not")
  (expect nil
    (ejep/service/string-match-fully-p "abc" "abcd"))

  (desc "glob-pattern-command-matcher")
  (expect '("*.text" "command1")
    (ejep/service/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text"))

  (desc "glob-pattern-command-matcher")
  (expect '("*.text2" "command2")
    (ejep/service/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text2"))

  (desc "glob-pattern-command-matcher")
  (expect nil
    (ejep/service/glob-pattern-command-matcher '(("*.text" "command1") ("*.text2" "command2")) "test.text3"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect '("*.test1" "command1")
    (ejep/service/glob-pattern-command-matcher-with-file "./.jep" "test.test1"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect '("*.test2" "command2")
    (ejep/service/glob-pattern-command-matcher-with-file "./.jep" "test.test2"))

  (desc "glob-pattern-command-matcher-with-file")
  (expect nil
    (ejep/service/glob-pattern-command-matcher-with-file "./.jep" "test.test3"))

  (desc "convinient get jepconfig with pattern and command")
  (expect (list (expand-file-name "./.jep") "*.test2" "command2") (ejep/service/get-jepconfig-with-pattern-and-command "./blub.test2"))

  (desc "convinient get jepconfig with pattern and command -> nil")
  (expect nil (ejep/service/get-jepconfig-with-pattern-and-command "./blub.test3"))

  (desc "key calc for jepfile and pattern")
  (expect "pattern@file" (ejep/service/calc-jep-and-pattern-key "file" "pattern"))

  ;; problems
  (desc "font calculation for severity")
  (expect "ejep/problems/faces/testerle" (ejep/problems/face-for-severity "testerle"))

  (desc "special number to string works for numbers and nil")
  (expect "" (ejep/problems/number-to-string nil))
  (expect "123" (ejep/problems/number-to-string 123))
)
