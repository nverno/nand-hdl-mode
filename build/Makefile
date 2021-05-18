CC       ?= g++
#$(CURDIR)/nand2tetris
NANDDIR  ?= ~/projects/nand2tetris
OUTFILE  ?= $(CURDIR)/docs.txt
LOC      =  $(CURDIR)
BUILTINS =  ${NANDDIR}/tools/builtInChips
EXT      =  

ifeq ($(OS), Windows_NT)
  EXT  = .exe
  LOC := $(shell cygpath -a $(LOC))
endif

ifeq ("$(wildcard $(NANDDIR))", "")
  $(error "Nand directory: $(NANDDIR) not found")
endif

DOCS =  $(CURDIR)/docs$(EXT)
HDL  := $(notdir $(wildcard $(BUILTINS)/*.hdl))

.PHONY: clean all
all: $(DOCS) run

run: $(BUILTINS) $(DOCS)
	cd $(BUILTINS) && \
		$(DOCS) $(OUTFILE) $(HDL)

$(BUILTINS):
	./provision.sh

.INTERMEDIATE: $(DOCS)
$(DOCS): docs.c
	$(CC) $< -o $@

clean:
	$(RM) *.exe *~ *.out
