*FFI-Kernel
unsignedLongLongAt: byteOffset put: value
	"I store 64-bit integers in Smalltalk (little-endian) order."
	^self integerAt: byteOffset put: value size: 8 signed: false