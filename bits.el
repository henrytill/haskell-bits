(defun ht/nix-shell-command (command)
  (format "%s \"%s\"" "nix-shell --command" command))

(defun ht/compile-in-nix-shell (command)
  (compile (ht/nix-shell-command command)))

(defun ht/async-shell-command-in-nix-shell (command)
  (async-shell-command (ht/nix-shell-command command)))

(defun ht/doctest ()
  (interactive)
  (ht/compile-in-nix-shell (concat "doctest " (buffer-file-name))))

(defun ht/haddock ()
  (interactive)
  (ht/compile-in-nix-shell (concat "haddock --hyperlinked-source -h -o doc " (buffer-file-name))))

(defun ht/run-hoogle ()
  (interactive)
  (ht/async-shell-command-in-nix-shell "hoogle server -p 8080")
  (setq haskell-hoogle-url "http://localhost:8080/?hoogle=%s")
  (browse-url "http://localhost:8080"))

(bind-map ht/playground-leader-map
  :keys ("M-m c")
  :evil-keys ("SPC c")
  :major-modes (haskell-mode))

(ht/comment
  (setq haskell-process-wrapper-function
        (lambda (argv) (append (list "nix-shell" "--command" )
                               (list (mapconcat 'identity argv " ")))))
  nil)

(bind-map-set-keys ht/playground-leader-map
  "d" 'ht/doctest
  "h" 'ht/haddock)
