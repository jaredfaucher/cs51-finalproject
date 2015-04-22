FILES = Primes.ml TmpFiniteShamirenc.ml TmpFiniteShamirdec.ml \

shamirenc: $(FILES)
	corebuild TmpFiniteShamirenc.native

shamirdec: $(FILES)
	corebuild TmpFiniteShamirdec.native

clean:
	rm -rf _build *.native
