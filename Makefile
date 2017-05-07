all:
	stack build

release:
	stack build --pedantic

example: all
	cd example; stack exec sd -- AdServer.hs -o AdServer.gml

install:
	stack install
