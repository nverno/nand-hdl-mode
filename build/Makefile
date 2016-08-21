cc ?= g++

loc=$(abspath .)
builtins= nand2tetris/tools/builtInChips
hdl=$(shell basename -a $(wildcard $(builtins)/*.hdl))
ext=

ifeq ($(OS), Windows_NT)
  ext=.exe
  loc:=$(shell cygpath -a $(loc))
endif

docs=docs$(ext)
nand= nand2tetris

.PHONY: clean all
all: $(docs) run

run: $(builtins)
	mv $(docs) $(builtins)
	cd $(builtins) && ./$(docs) $(hdl)
	mv $(builtins)/docs.txt ..

$(builtins) :
	./provision.sh

$(docs) : docs.c
	$(cc) $< -o $@

clean:
	$(RM) *.exe *~ *.out
