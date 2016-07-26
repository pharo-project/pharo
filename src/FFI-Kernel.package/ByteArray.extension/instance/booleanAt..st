*FFI-Kernel
booleanAt: byteOffset
	"Booleans are just integers in C word"
	^(self integerAt: byteOffset size: 1 signed: false) == true