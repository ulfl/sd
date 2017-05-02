all:
	stack build

release:
	stack build --pedantic

example: all
	cd example; stack exec sd -- adserver.hs -o adserver.gml

install:
	stack install
