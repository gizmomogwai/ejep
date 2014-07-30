task :test do
  sh 'emacs -Q -batch -L ~/.emacs.d/el-get/ert-expectations -L ~/.emacs.d/el-get/el-mock -l ejep.el -f ert-run-tests-batch-and-exit'
end

task :default => [:test]
