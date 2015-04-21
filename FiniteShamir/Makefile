FILES = Primes.ml TmpFiniteShamirenc.ml TmpFiniteShamirdec.ml \

all: shamirenc shamirdec
	corebuild TmpFiniteShamirenc.native
	corebuild TmpFiniteShamirdec.native

shamirenc: $(FILES)
	corebuild TmpFiniteShamirenc.native

shamirdec: $(FILES)
	corebuild TmpFiniteShamirdec.native

clean:
	rm -rf _build *.native
