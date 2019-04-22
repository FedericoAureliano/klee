CILLY=cilly
CLANG=clang
KLEE=klee
COPTS=-Wno-attributes
INSTKLEE=../../_build/instKlee.cma
# if instKlee has been installed, you can also use:
# INSTKLEE=instKlee

export CIL_FEATURES=cil.oneret

.PHONY: all clean

foo: foo.c
	$(CILLY) $(COPTS) --save-temps --noPrintLn -c --load=$(INSTKLEE) --doinstKlee --entry=foo foo.c
	$(CLANG) -emit-llvm -c foo.cil.c
	$(KLEE) -entry-point=foo foo.cil.bc 

get_sign: get_sign.c
	$(CILLY) $(COPTS) --save-temps --noPrintLn -c --load=$(INSTKLEE) --doinstKlee --entry=get_sign --assume="x == 2" get_sign.c
	$(CLANG) -emit-llvm -c get_sign.cil.c
	$(KLEE) -entry-point=get_sign get_sign.cil.bc 

clean:
	rm -rf *.o *.i *.cil.* klee-*