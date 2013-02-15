export LC_CTYPE=el_GR.utf8

.PHONY: default build rebuild preview deploy hook clean distclean

default: rebuild

build: hakyll
	./hakyll build

rebuild: hakyll
	./hakyll rebuild

preview: hakyll
	./hakyll preview

deploy: build
	rsync -aH --delete _site/ $(HOME)/www/tmp/testpub/

hook: deploy distclean

hakyll:	hakyll.hs
	ghc --make $@

clean:
	$(RM) *.hi *.o *~

distclean: clean
	-./hakyll clean
	$(RM) hakyll
