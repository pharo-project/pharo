instruction stream support
isJustPopAt: pc in: method
	"Answer whether the bytecode at pc is a pop."

	^(method at: pc) = 216 "216		11011000			Pop Stack Top"