command DocTest !nix-shell --command "doctest %:t"
command Haddock !nix-shell --command "haddock --hyperlinked-source -h -o doc %:t"
nnoremap <leader>d :DocTest<CR>
nnoremap <leader>h :Haddock<CR>
