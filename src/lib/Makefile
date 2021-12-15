OCAMLMAKEFILE = ../make/OCamlMakefile

SOURCES = version.mli version.ml datalog.mli datalog.ml sexpr.mli	\
sexpr.ml cpplSig.mli cpplSig.ml cpplFunctor.mli cpplFunctor.ml		\
crypto.mli crypto.ml cppl.mli cppl.ml

RESULT  = cppl

PACKS = unix cryptokit

LIBINSTALL_FILES = $(RESULT).mli $(RESULT).cmi $(RESULT).cma	\
$(RESULT).cmxa $(RESULT).$(EXT_LIB) cpplSig.cmi cpplSig.mli	\
cpplFunctor.cmi cpplFunctor.mli sexpr.cmi sexpr.mli crypto.cmi	\
crypto.mli

all:	dcl ncl

install:	libinstall

uninstall:	libuninstall

include $(OCAMLMAKEFILE)
