EMACS    ?= emacs
WGET     ?= wget
ELPA_DIR ?= ~/.emacs.d/elpa
AUTO     ?= nand-hdl-mode-autoloads.el

el  = $(filter-out $(AUTO) .dir-locals.el,$(wildcard *.el))
elc = $(el:.el=.elc)

BATCH_FLAGS = -batch                                       \
	--eval "(let ((default-directory                   \
                      (expand-file-name \"$(ELPA_DIR)\"))) \
                  (normal-top-level-add-subdirs-to-load-path))"

AUTO_FLAGS ?=                                                          \
	--eval "(progn (defvar generated-autoload-file nil)            \
		  (let ((generated-autoload-file                       \
			(expand-file-name (unmsys--file-name \"$@\"))) \
		        (wd (expand-file-name default-directory))      \
		        (backup-inhibited t)                           \
		        (default-directory                             \
			  (expand-file-name \"$(ELPA_DIR)\")))         \
		    (normal-top-level-add-subdirs-to-load-path)        \
		    (update-directory-autoloads wd)))"

.PHONY: $(AUTO) clean distclean
all: compile $(AUTO)

compile : $(elc)
%.elc : %.el
	$(EMACS) $(BATCH_FLAGS) -f batch-byte-compile $<

$(AUTO):
	$(EMACS) -batch $(AUTO_FLAGS)

README.md: el2markdown.el nand-hdl-mode.el
	$(EMACS) -batch -l $< nand-hdl-mode.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(WGET) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

TAGS: $(el)
	$(RM) $@
	touch $@
	ls $(el) | xargs etags -a -o $@

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
