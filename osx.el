; Set up Erlang from Homebrew

(setq load-path (cons "/usr/local/Cellar/erlang/R15B02/lib/erlang/lib/tools-2.6.8/emacs" load-path))
(setq erlang-root-dir "/usr/local/Cellar/erlang/R15B02")
(setq exec-path (cons "/usr/local/Cellar/erlang/R15B02/bin" exec-path))
(require 'erlang-start)
(require 'erlang-flymake)
