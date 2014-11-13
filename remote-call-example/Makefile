PACKAGES=ctypes.stubs
GENERATOR=./ex_generator.native
TARGETS=ex_remote_stubs.c ex_stubs.c ex_generated.ml ex_shared_definitions.h ex_remote_server
INCPATH=$(shell ocamlfind -query ctypes)/..
OCAMLPATH=$(shell ocamlc -where)
CFLAGS=-ggdb -std=c99 -Wall -fPIC -I$(INCPATH) -I$(OCAMLPATH)
OCAML_LDFLAGS=$(LDFLAGS:%=-cclib %)
OSR=$(shell uname)
ifeq ($(OSR),Linux) 
EXTRA_LINK_FLAGS=-lrt -lpthread
endif

all: ex_example.native ex_remote_server

%.cmx: %.ml
	ocamlfind opt -c -package $(PACKAGES) $^

%.o: %.c
	$(CC) -o $@ -c $(CFLAGS) $^

%.native:
	ocamlfind opt -o $@ -linkpkg -package $(PACKAGES) $^ $(OCAML_LDFLAGS)

libex.so: ex.o
	$(CC) -o $@ -shared $^

$(GENERATOR): ex_bindings.cmx ex_generator.cmx

ex_remote_stubs.c: $(GENERATOR) ex_shared_definitions.h
	$(GENERATOR) --c-remote-file $@

ex_shared_definitions.h: $(GENERATOR)
	$(GENERATOR) --h-file $@

ex_stubs.c: $(GENERATOR) ex_shared_definitions.h
	$(GENERATOR) --c-file $@

ex_generated.ml: $(GENERATOR)
	$(GENERATOR) --ml-file $@

ex_remote_server: LDFLAGS=-L. -lex -lm $(EXTRA_LINK_FLAGS)
ex_remote_server: ex_remote_stubs.c server_stub.c libex.so
	$(CC) -o $@ $(CFLAGS) ex_remote_stubs.c server_stub.c $(LDFLAGS)

ex_example.native: LDFLAGS=$(EXTRA_LINK_FLAGS)
ex_example.native: ex_generated.cmx ex_stubs.o ex_bindings.cmx ex_main.cmx

clean:
	rm -f $(TARGETS) *.cmx *.cmo *.cmi *.native *.o *.so

.PHONY: clean
