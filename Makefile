.PHONY: build
build:
	cabal build all

.PHONY: run
run: build
	cabal run hdns-exe -- --port 1053 --records examples/records.zone

.PHONY: test
test:
	cabal test all
