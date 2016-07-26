*FFI-Kernel
unsignedShortAt: byteOffset
	"Return a 16bit unsigned integer starting at the given byte offset"
	^self integerAt: byteOffset size: 2 signed: false