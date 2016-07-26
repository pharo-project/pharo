*FFI-Kernel
signedShortAt: byteOffset
	"Return a 16bit signed integer starting at the given byte offset"
	^self integerAt: byteOffset size: 2 signed: true