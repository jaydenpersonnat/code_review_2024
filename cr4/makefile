all: cr4 cr4_tests 

cr3: cr4.ml
	ocamlbuild -use-ocamlfind cr4.byte

cr3_tests: cr4_tests.ml
	ocamlbuild -use-ocamlfind cr4_tests.byte

clean:
	rm -rf _build *.byte