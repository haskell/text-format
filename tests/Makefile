ghc := ghc

all: bm

bm: Benchmarks.hs
	$(ghc) -rtsopts -O -o $@ $<

clean:
	-rm -f bm *.hi *.o *.hp
