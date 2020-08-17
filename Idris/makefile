dmmf : dmmf.ipkg
	idris2 --build dmmf.ipkg

run : ./build/exec/DMMF
	./build/exec/DMMF

./build/exec/DMMF : dmmf.ipkg

repl : dmmf.ipkg
	idris2 --repl dmmf.ipkg

clean :
	rm -rf build/*
