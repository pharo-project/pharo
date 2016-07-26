*FFI-Kernel
unsignedByteAt: byteOffset
	"Return a 8bit unsigned integer starting at the given byte offset"
	^self integerAt: byteOffset size: 1 signed: false