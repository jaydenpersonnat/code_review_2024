all: cr4 cr4_tests 

cr4: cr4.ml
	ocamlbuild -use-ocamlfind cr4.byte

cr4_tests: cr4_tests.ml
	ocamlbuild -use-ocamlfind cr4_tests.byte

clean:
	rm -rf _build *.byte