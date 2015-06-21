default: ConfigParser.hs samesame.hs
	ghc -o samesame ConfigParser.hs samesame.hs

clean:
	rm *.hi *.o samesame
