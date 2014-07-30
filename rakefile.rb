module OS
  def self.osx?
    /darwin/ =~ RUBY_PLATFORM
  end
  def self.linux?
    puts RUBY_PLATFORM
  end
end

def emacs
  return '/Applications/Emacs.app/Contents/MacOS/Emacs' if OS.osx?
  return 'emacs'
end

desc 'run the tests'
task :test do
  sh "cask exec ert-runner"
end

desc 'run emacs'
task :run do
  sh "cask exec #{emacs} -L ."
end

task :default => [:test]
