all:
	stack build

release:
	stack build --pedantic

run:
	stack exec sd

install:
	stack install
