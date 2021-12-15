OCAMLMAKEFILE = ../make/OCamlMakefile

SOURCES = version.mli version.ml pos.mli pos.ml ast.ml parser.mly	\
scanner.mli scanner.mll reader.mli reader.ml scope.mli scope.ml		\
infer.mli infer.ml pretty.mli pretty.ml scheme.mli scheme.ml		\
message.mli message.ml printer.mli printer.ml axioms.mli axioms.ml	\
cpplc.ml

RESULT  = cpplc

ifndef bindir
  bindir := /usr/local/bin
endif

ifndef INSTALL_PROGRAM
  INSTALL_PROGRAM := install -c
endif

all:	dc

install:	all
	test -z "$(bindir)" || mkdir -p -- "$(bindir)"
	$(INSTALL_PROGRAM) $(RESULT) $(bindir)

uninstall:
	rm -f $(bindir)/$(RESULT)

include $(OCAMLMAKEFILE)
