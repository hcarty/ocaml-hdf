# The target library's name
LIBRARY = hdf

# Commands to use for ocamlbuild and ocamlfind (in case they are not in $PATH)
OCAMLBUILD = ocamlbuild -tag debug
OCAMLFIND = ocamlfind

export CAMLIDL_LIB_DIR = -L$(shell ocamlc -where)
export CAMLIDL_LIB = -lcamlidl
export HDF_CFLAGS = -I/usr/include
export HDF_LIBS_DIRS = -L/usr/lib
export HDF_LIBS_LIBS = -lmfhdf -ldf -lz -ljpeg
export HDF_LIBS = $(HDF_LIBS_DIRS) $(HDF_LIBS_LIBS)

# Where ocamlbuild put the build files
BUILD_DIR = _build/

# Default to building bytecoode and native code libraries
all: byte opt

byte:
	$(OCAMLBUILD) $(LIBRARY).cma

opt:
	$(OCAMLBUILD) $(LIBRARY).cmxa

# (Un)Installation using ocamlfind
install:
	$(OCAMLFIND) install $(LIBRARY) \
	    META \
	    $(BUILD_DIR)*hdf.cmi \
	    $(BUILD_DIR)*hdf.cma \
	    $(BUILD_DIR)*hdf.cmxa \
	    $(BUILD_DIR)*hdf_stubs.so \
	    $(BUILD_DIR)*hdf_stubs.a \
	    $(BUILD_DIR)*hdf.a

uninstall:
	$(OCAMLFIND) remove $(LIBRARY)

# Clean up the build process using ocamlbuild
clean:
	$(OCAMLBUILD) -clean

