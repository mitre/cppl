CPPL_HOME=../..

export OCAMLMAKEFILE = $(CPPL_HOME)/src/make/OCamlMakefile

export PACKS = unix cryptokit

define PROJ_mkks
  SOURCES = mkks.ml
  RESULT = mkks
  INCDIRS = $(CPPL_HOME)/src/lib
  LIBS = $(CPPL_HOME)/src/lib/cppl
endef
export PROJ_mkks

ifndef SUBPROJS
  export SUBPROJS = mkks
endif

all:	bc

install:
	@echo install is up to date.

uninstall:
	@echo uninstall is up to date.

%:
	@make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
