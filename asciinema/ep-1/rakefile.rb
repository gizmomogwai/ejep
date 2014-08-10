task :default do
  sh 'asciinema -t "introducing ejep" -c "emacs -nw title.txt comments.org"'
end
