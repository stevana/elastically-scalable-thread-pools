((haskell-cabal-mode
  (eval .
        (add-hook 'before-save-hook
                  (lambda () (haskell-mode-buffer-apply-command "cabal-fmt")) nil t))))
