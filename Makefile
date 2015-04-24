all:  main
	corebuild main.native

main:
	corebuild main.native

clean:
	rm -rf _build *.native
