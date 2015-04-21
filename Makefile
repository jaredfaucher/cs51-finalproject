all: shamirtryenc shamirtrydec main
	corebuild shamirtryenc.native
	corebuild shamirtrydec.native
	corebuild main.native

shamirtryenc:
	corebuild shamirtryenc.native

shamirtrydec:
	corebuild shamirtrydec.native

main:
	corebuild main.native

clean:
	rm -rf _build *.native
