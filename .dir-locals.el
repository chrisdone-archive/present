((haskell-mode
  . ((haskell-process-type . ghci)
     (haskell-process-use-ghci . t)
     (haskell-process-path-ghci . "stack")
     (haskell-process-args-ghci . ("ghci" "--with-ghc" "ghci-ng"))))
 (haskell-cabal-mode
  . ((haskell-process-type . ghci)
     (haskell-process-path-ghci . "stack")
     (haskell-process-args-ghci . ("ghci" "--with-ghc" "ghci-ng")))))
