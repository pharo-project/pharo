*FFI-Kernel
signedShortAt: byteOffset put: value
	"Store a 16bit signed integer starting at the given byte offset"
	^self integerAt: byteOffset put: value size: 2 signed: true