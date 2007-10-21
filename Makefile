OCAMLMAKEFILE = /home/hcarty/Applications/godi/share/OCamlMakefile
ANNOTATE = yes
PACKS = bigarray extbigarray
LIBS =
OCAMLLIBPATH =
INCDIRS=/home/hcarty/Applications/hdf4/include
LIBDIRS=/home/hcarty/Applications/hdf4/lib
EXTLIBDIRS =

CLIBS = mfhdf df z jpeg
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

mrproper: clean
	rm -f *~ *.cmi *.cmo *.top *.so hdf_h.inc mfhdf_h.inc

.PHONY: mrproper

include $(OCAMLMAKEFILE)
