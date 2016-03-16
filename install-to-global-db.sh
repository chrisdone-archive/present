# Installs globally to your GHC's global package database

stack install

stack exec --no-ghc-package-path -- runhaskell Setup.hs clean
stack exec --no-ghc-package-path -- runhaskell Setup.hs configure --global --package-db $(stack path --global-pkg-db)
stack exec --no-ghc-package-path -- runhaskell Setup.hs build
stack exec --no-ghc-package-path -- runhaskell Setup.hs install
