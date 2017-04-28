.SUFFIXES: .F90 .o
extension = .F90

SRC_DIR = .
OBJ_DIR = .
LIB_DIR = .
INC_DIR = .
FC = NONE

# Dependency Generation
MAKE_DEP = $(SRC_DIR)/makedep.py
DEP_FILE = $(OBJ_DIR)/shared_deps.d

# Flags used for error checking
# (1) Make sure FC is specified
ifeq ($(FC),NONE)
  NOFC = TRUE
endif
# (2) If building libcvmix-dyn.so, make sure FC=gfortran
ifneq ($(findstring dynlib,$(MAKECMDGOALS)),)
ifneq ($(FC),gfortran)
  DYNLIB_NO_GFORT=TRUE
endif
endif

MODULES = cvmix_kinds_and_types.F90 \
          cvmix_background.F90 \
          cvmix_convection.F90 \
          cvmix_ddiff.F90 \
          cvmix_kpp.F90 \
          cvmix_math.F90 \
          cvmix_put_get.F90 \
          cvmix_shear.F90 \
          cvmix_tidal.F90 \
          cvmix_utils.F90

# Some compilers produce ALL_UPPER_CASE.mod files
ifeq ($(UCASE),TRUE)
  MODS_TMP = $(shell echo $(MODULES) | tr '[a-z]' '[A-Z]')
else
  MODS_TMP = $(MODULES)
endif
ifneq ($(OBJ_DIR),$(INC_DIR))
  INCS = $(addprefix $(INC_DIR)/,${MODS_TMP:.F90=.mod})
endif
MODS = $(addprefix $(OBJ_DIR)/,${MODS_TMP:.F90=.mod}) \
       $(INCS)
OBJS = $(addprefix $(OBJ_DIR)/,${MODULES:.F90=.o})

ifeq ($(USE_DEPS),TRUE)
  include $(DEP_FILE)
endif

### TARGETS ###

all: lib

# Create all object and module files
# Note that .mod files need to be copied to INC_DIR if OBJ_DIR != INC_DIR

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.F90
	$(FC) $(FCFLAGS) -c $< -o $@

$(INC_DIR)/%.mod: $(OBJ_DIR)/%.mod
ifneq ($(INC_DIR),$(OBJ_DIR))
	cp $< $@
endif

### Static library
$(LIB_DIR)/libcvmix.a: $(OBJS)
	ar -ru $(LIB_DIR)/libcvmix.a $(OBJS)

### Dynamic Library
$(LIB_DIR)/libcvmix-dyn.so: $(OBJS)
  # this is meant for use with gfortran only (for now)
	$(FC) -shared $(OBJS) -o $(LIB_DIR)/libcvmix-dyn.so

$(DEP_FILE): $(MAKE_DEP) $(SRC_DIR)/*.F90
	$(MAKE_DEP) $(DEP_FILE) $(OBJ_DIR) $(SRC_DIR)
	@echo "Generated dependencies!"

.PHONY: depends lib dynlib recurse check clean

# Shorthand for making dependency file
depends: $(DEP_FILE)

# Shorthand for making the static library (and all .mod files)
lib: check depends
	$(MAKE) -e -f $(SRC_DIR)/Makefile $(LIB_DIR)/libcvmix.a $(INCS) USE_DEPS=TRUE

# Shorthand for making the dynamic library (and all .mod files)
dynlib: check depends
	$(MAKE) -e -f $(SRC_DIR)/Makefile $(LIB_DIR)/libcvmix-dyn.so $(INCS) USE_DEPS=TRUE

# Make sure a Fortran compiler was specified (the Makefile for the stand-alone
# driver passes FC along with FCFLAGS, SRC_DIR, OBJ_DIR, LIB_DIR, and INC_DIR)
check:
	@$(if $(NOFC), echo "ERROR: you must specify FC (and it is recommended that \
     you specify FCFLAGS as"; echo "well)."; echo "NOTE: if you have checked  \
     out the stand-alone CVMix driver set then you should"; echo "run \"make  \
     lib\" from the src/ directory to use CVMix compile options."; exit 1)
	@$(if $(DYNLIB_NO_GFORT), echo "ERROR: Can only build dynamic library with  \
     gfortran."; exit 1)

# Remove library, object files, module files, and dependency file
clean:
	/bin/rm -f $(LIB_DIR)/libcvmix.a $(LIB_DIR)/libcvmix-dyn.so $(OBJS) $(MODS) $(DEP_FILE)

