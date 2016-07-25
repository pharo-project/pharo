*FFI-Kernel
signedLongAt: byteOffset
	"Return a 32bit signed integer starting at the given byte offset"
	^self integerAt: byteOffset size: 4 signed: true