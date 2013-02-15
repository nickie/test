export LC_CTYPE=el_GR.utf8

.PHONY: default build rebuild preview hook clean distclean

default: rebuild

build: hakyll
	./hakyll build

rebuild: hakyll
	./hakyll rebuild

preview: hakyll
	./hakyll preview

hook: hakyll
	./hakyll build
#	./hakyll deploy

hakyll:	hakyll.hs
	ghc --make $@

clean:
	$(RM) *.hi *.o *~

distclean: clean
	-./hakyll clean
	$(RM) hakyll
