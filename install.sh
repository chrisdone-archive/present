stack exec --no-ghc-package-path -- runhaskell Setup.hs configure --global --package-db /Users/chris/.stack/programs/x86_64-osx/ghc-7.10.3/lib/ghc-7.10.3/package.conf.d
stack exec --no-ghc-package-path -- runhaskell Setup.hs build
stack exec --no-ghc-package-path -- runhaskell Setup.hs install
