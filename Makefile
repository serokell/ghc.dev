.PHONY: clean
out:
	cabal v2-run ghc-dev-webgen out
clean:
	rm -r out
