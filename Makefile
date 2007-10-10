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

SOURCES = hdf.mli hdf.ml hdf_stubs.c

all: byte-code-library native-code-library top

mrproper: clean
	rm -f *~ *.cmi *.cmo *.top *.so

.PHONY: mrproper

include $(OCAMLMAKEFILE)
