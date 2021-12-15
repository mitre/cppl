CPPL_HOME=../../..

SHARED = globals.ml sockaddr.ml load_key_store.ml protocol.ml

export OCAMLMAKEFILE = $(CPPL_HOME)/src/make/OCamlMakefile

export PACKS = unix cryptokit

define PROJ_init
  SOURCES = $(SHARED) init_axioms.ml init.ml
  RESULT = init
  INCDIRS = $(CPPL_HOME)/src/lib
  LIBS = $(CPPL_HOME)/src/lib/cppl
endef
export PROJ_init

define PROJ_resp
  SOURCES = $(SHARED) resp_axioms.ml resp.ml
  RESULT = resp
  INCDIRS = $(CPPL_HOME)/src/lib
  LIBS = $(CPPL_HOME)/src/lib/cppl
endef
export PROJ_resp

define PROJ_ca
  SOURCES = $(SHARED) ca.ml
  RESULT = ca
  INCDIRS = $(CPPL_HOME)/src/lib
  LIBS = $(CPPL_HOME)/src/lib/cppl
endef
export PROJ_ca

define PROJ_mkks
  SOURCES = $(SHARED) mkks.ml
  RESULT = mkks
  INCDIRS = $(CPPL_HOME)/src/lib
  LIBS = $(CPPL_HOME)/src/lib/cppl
endef
export PROJ_mkks

ifndef SUBPROJS
  export SUBPROJS = init resp ca mkks
endif

all:	dc ac_nsl

clean:
	rm -f ac_nsl
	@make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=clean

install:
	@echo install is up to date.

uninstall:
	@echo uninstall is up to date.

%:
	@make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
