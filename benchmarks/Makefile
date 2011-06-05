ghc := ghc

programs := bm simple sprintf swprintf

all: $(programs)

bm: Benchmarks.hs
	$(ghc) -rtsopts -O -o $@ $<

simple: Simple.hs
	$(ghc) -rtsopts -O -o $@ $<

sprintf: sprintf.c
	$(CC) -O2 -o $@ $<

swprintf: swprintf.c
	$(CC) -O2 -o $@ $<

clean:
	-rm -f $(programs) *.hi *.o *.hp
