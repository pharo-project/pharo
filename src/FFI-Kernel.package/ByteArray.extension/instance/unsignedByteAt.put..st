*FFI-Kernel
unsignedByteAt: byteOffset put: value
	"Store a 8bit unsigned integer starting at the given byte offset"
	^self integerAt: byteOffset put: value size: 1 signed: false