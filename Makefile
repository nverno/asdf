wget  ?= wget
RM    = rm -rf
emacs ?= emacs

.PHONY: test clean distclean
all: test
test: 
	$(emacs) -Q -batch                    \
	--eval '(progn (push "." load-path))' \
	-L . -l ert -l test/asdf-tests.el -f ert-run-tests-batch-and-exit

README.md: el2markdown.el asdf.el
	$(emacs) -batch -l $< asdf.el         \
	-f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
