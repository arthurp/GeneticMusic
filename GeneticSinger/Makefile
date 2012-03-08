all : TACCompiler TAC.hs tacrts.c
	./TACCompiler
	$(MAKE) TACTest.ll
	$(MAKE) TACTest
	./TACTest 64000 > TACTest.raw
	sox -s -e float -b 64 -c 1 -r 16k -v 0.1 TACTest.raw -t wav -e signed -b 16 TACTest.wav
	play TACTest.wav

%.ll : %.bc
	llvm-dis $^

% : %.hs TAC.hs
	ghc --make $^

%.s : %.bc
	llc --enable-unsafe-fp-math -O=3 $^ -o $@

% : %.s
	gcc $^ tacrts.c -lm -o $@

