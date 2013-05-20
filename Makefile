all:
	ocamlbuild -use-ocamlfind src/main.native

byte:
	ocamlbuild -use-ocamlfind src/main.byte

clean:
	rm -rf _build
