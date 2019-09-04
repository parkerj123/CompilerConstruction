all:
	happy -gca ParCpp.y
	alex -g LexCpp.x
	ghc --make TestCpp.hs -o TestCpp

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocCpp.* LexCpp.* ParCpp.* LayoutCpp.* SkelCpp.* PrintCpp.* TestCpp.* AbsCpp.* TestCpp ErrM.* SharedString.* ComposOp.* Cpp.dtd XMLCpp.* Makefile
	
