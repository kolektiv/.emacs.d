; Set up Erlang from Homebrew

(setq load-path (cons "/usr/local/Cellar/erlang/R15B/lib/erlang/lib/tools-2.6.6.6/emacs" load-path))
(setq erlang-root-dir "/usr/local/Cellar/erlang/R15B")
(setq exec-path (cons "/usr/local/Cellar/erlang/R15B/bin" exec-path))
(require 'erlang-start)
(require 'erlang-flymake)
