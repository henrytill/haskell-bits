((haskell-mode . ((haskell-process-type . ghci)
                  (haskell-process-wrapper-function . (lambda (argv)
                                                        (append (list "nix-shell" "--command" )
                                                                (list (mapconcat 'identity argv " ")))))
                  )))
