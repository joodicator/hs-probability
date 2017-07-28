.phony: all
all:
	ghc -O2 Risk.hs

.phony: clean
clean:
	rm -f *.hi *.o Risk
