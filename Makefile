OCAMLMAKEFILE = ../inc/OCamlMakefile
ANNOTATE = yes
PACKS = bigarray extbigarray pcre mylib
LIBS =
OCAMLLIBPATH =
INCDIRS=/usr/include/hdf
LIBDIRS=/usr/lib/hdf $(OCAMLLIBPATH)
EXTLIBDIRS =

CLIBS = mfhdf df z jpeg camlidl
CFLAGS = -g

# We turn on debugger support in all our modules for now.
OCAMLBCFLAGS =
OCAMLBLDFLAGS =
RESULT = hdf

SOURCES = hdf_wrapper.idl hdf_impl.c hdf.ml

all: includes byte-code-library native-code-library top

includes:
	perl touchup.pl hdf_h
	perl touchup.pl mfhdf_h

interface:
	ocamlfind ocamlc -package extbigarray,pcre,mylib -i hdf.ml > hdf.mli

install: all interface libinstall

mrproper: clean
	rm -f *~ *.cmi *.cmo *.top *.so hdf_h.inc mfhdf_h.inc

.PHONY: mrproper

include $(OCAMLMAKEFILE)
