(defun ht/compile-in-nix-shell (command)
  (compile (format "%s \"%s\"" "nix-shell --command" command)))

(defun ht/doctest ()
  (interactive)
  (ht/compile-in-nix-shell (concat "doctest " (buffer-file-name))))

(defun ht/haddock ()
  (interactive)
  (ht/compile-in-nix-shell (concat "haddock --hyperlinked-source -h -o doc " (buffer-file-name))))

(bind-map ht/playground-leader-map
  :keys ("M-m c")
  :evil-keys ("SPC c"))

(bind-map-set-keys ht/playground-leader-map
  "d" 'ht/doctest
  "h" 'ht/haddock)
