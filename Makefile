FILES=\
	main.d

LLVM_LIBS=`llvm-config --libs | sed 's/-l/-L-l/g'`
LLVM_OBJ=

main: $(FILES)
	dmd -ofmain -L-L/usr/local/lib -L-ldl -L-lstdc++ $(FLAGS) -fPIC $(FILES) $(LLVM_LIBS) $(LLVM_OBJ)
