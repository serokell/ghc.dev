Specify a path to GHC in `cabal.project.local`, for example:

```
with-compiler: /Users/int-index/.stack/programs/x86_64-osx/ghc-8.6.4/bin/ghc
```

Then generate `out/index.html` with:

```
make
```

For a different output directory `CUSTOM_OUT`:

```
cabal v2-run ghc-dev-webgen CUSTOM_OUT
```
