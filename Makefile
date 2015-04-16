FILES = encrypt.ml Shamirint.ml main.ml

shamir: $(FILES)
	corebuild -lib str main.native

clean:
	rm -rf _build main.native
