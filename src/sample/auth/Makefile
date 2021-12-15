CPPL_HOME=../../..

SHARED = self.ml load_key_store.ml protocol.ml

export OCAMLMAKEFILE = $(CPPL_HOME)/src/make/OCamlMakefile

export PACKS = unix cryptokit

define PROJ_init
  SOURCES = sockaddr.ml $(SHARED) init_axioms.ml init.ml
  RESULT = init
  INCDIRS = $(CPPL_HOME)/src/lib
  LIBS = $(CPPL_HOME)/src/lib/cppl
endef
export PROJ_init

define PROJ_resp
  SOURCES = $(SHARED) resp.ml
  RESULT = resp
  INCDIRS = $(CPPL_HOME)/src/lib
  LIBS = $(CPPL_HOME)/src/lib/cppl
endef
export PROJ_resp

define PROJ_ca
  SOURCES = $(SHARED) ca_axioms.ml ca.ml
  RESULT = ca
  INCDIRS = $(CPPL_HOME)/src/lib
  LIBS = $(CPPL_HOME)/src/lib/cppl
endef
export PROJ_ca

ifndef SUBPROJS
  export SUBPROJS = init resp ca
endif

all:	dc auth

clean:
	rm -f auth
	@make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=clean

install:
	@echo install is up to date.

uninstall:
	@echo uninstall is up to date.

%:
	@make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
