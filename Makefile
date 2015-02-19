
all: pyparse


# Translates the Python grammar in the Python spec 
# into an derp-comptabile version of the grammar:
python.grammar.sx: python.grammar
	racket pygram2sxgram.rkt python.grammar

# Makes a copy of the python grammar for marking it up:
my-python.grammar.sx: python.grammar.sx
	@if [ -f my-python.grammar.sx ]; then echo "warning: my-python.grammar.sx exists and may have been modified by hand; are you sure you want to overwrite?"; exit -1; fi
	cp python.grammar.sx my-python.grammar.sx

# Compiles the modified derp-compatible version of the 
# Python grammar into a Racket yacc-compatible format:
my-python.yacc.sx: my-python.grammar.sx sxgram2yaccgram.rkt
	racket sxgram2yaccgram.rkt my-python.grammar.sx

# Stitch together the parser:
pyparse.rkt: my-python.yacc.sx pyparse-prefix.rkt pyparse-suffix.rkt
	cat pyparse-prefix.rkt my-python.yacc.sx pyparse-suffix.rkt > pyparse.rkt


pyparse: pyparse.rkt
	raco exe pyparse.rkt

tests:
	cd tests-exp; for i in *.py; do pylex --exp < $$i > $$(basename $$i .py).toks; done
	cd tests-stmt; for i in *.py; do pylex < $$i > $$(basename $$i .py).toks; done

.PHONY: clean
clean:
	rm -fv pyparse pyparse.rkt *.yacc.sx python.grammar.sx tests-exp/*.toks tests-stmt/*.toks


