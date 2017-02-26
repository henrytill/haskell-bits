command DocTest !nix-shell -p haskellPackages.doctest --command "doctest %:t"
command Haddock !nix-shell -p haskellPackages.haddock --command "haddock --hyperlinked-source -h -o doc %:t"
nnoremap <leader>d :DocTest<CR>
nnoremap <leader>h :Haddock<CR>
