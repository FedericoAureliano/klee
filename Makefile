.PHONY: all install uninstall clean check

all:
	ocamlbuild -classic-display -use-ocamlfind instKlee.cma instKlee.cmxs

install: uninstall all
	ocamlfind install instKlee META _build/instKlee.cma _build/instKlee.cmxs

uninstall:
	ocamlfind remove instKlee

clean:
	ocamlbuild -clean
	$(MAKE) -C examples/gen clean

check: all
	$(MAKE) -C examples/gen
