GHCFLAGS=-Wall -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.1

.PHONY: all shell clean doc install

all: report.html doc dist/build/libHSripple-$(VERSION).a dist/ripple-$(VERSION).tar.gz

install: dist/build/libHSripple-$(VERSION).a
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: Ripple/Amount.hs Ripple/Seed.hs Ripple/Transaction.hs Ripple/WebSockets.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/ripple/index.html README

README: ripple.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/ripple/index.html: dist/setup-config Ripple/Amount.hs Ripple/Seed.hs Ripple/Transaction.hs Ripple/WebSockets.hs
	cabal haddock --hyperlink-source

dist/setup-config: ripple.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/libHSripple-$(VERSION).a: dist/setup-config Ripple/Amount.hs Ripple/Seed.hs Ripple/Transaction.hs Ripple/WebSockets.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/ripple-$(VERSION).tar.gz: README dist/setup-config Ripple/Amount.hs Ripple/Seed.hs Ripple/Transaction.hs Ripple/WebSockets.hs
	cabal check
	cabal sdist
