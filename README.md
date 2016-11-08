# echo-client
```bash
stack new my-project servant
cd new my-project (atom . )
stack setup
stack build
stack exec my-project-exe
```


# refresh
```bash
rm -rf ~/.stack/
rm -rf ~/.ghc/
rm -rf ./.stack-work/
stack setup && stack install cabal-install && stack build
```
