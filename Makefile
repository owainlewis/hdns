.PHONY: build
build:
	stack build

.PHONY: run
run: build
	sudo ./.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/hdns-exe/hdns-exe
