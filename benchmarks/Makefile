ghc := ghc

programs := bm simple c-printf c-wprintf

all: $(programs)

bm: Benchmarks.hs
	$(ghc) -rtsopts -O -o $@ $<

simple: Simple.hs
	$(ghc) -rtsopts -O -o $@ $<

c-printf: printf.c
	$(CC) -O2 -o $@ $<

c-wprintf: wprintf.c
	$(CC) -O2 -o $@ $<

clean:
	-rm -f $(programs) *.hi *.o *.hp
