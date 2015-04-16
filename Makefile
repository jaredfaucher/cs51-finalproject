all: shamirtryenc shamirtrydec
	corebuild shamirtryenc.native
	corebuild shamirtrydec.native

shamirtryenc:
	corebuild shamirtryenc.native

shamirtrydec:
	corebuild shamirtrydec.native

clean:
	rm -rf _build *.native
