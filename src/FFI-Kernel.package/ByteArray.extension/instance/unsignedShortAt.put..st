*FFI-Kernel
unsignedShortAt: byteOffset put: value
	"Store a 16bit unsigned integer starting at the given byte offset"
	^self integerAt: byteOffset put: value size: 2 signed: false