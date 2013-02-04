.PHONY: default build rebuild preview clean distclean

default: rebuild

build: hakyll
	./hakyll build

rebuild: hakyll
	./hakyll rebuild

preview: hakyll
	./hakyll preview

hakyll:	hakyll.hs
	ghc --make $@

clean:
	$(RM) *.hi *.o *~

distclean: clean
	-./hakyll clean
	$(RM) hakyll
