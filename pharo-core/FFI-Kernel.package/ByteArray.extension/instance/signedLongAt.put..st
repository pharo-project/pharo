*FFI-Kernel
signedLongAt: byteOffset put: value
	"Store a 32bit signed integer starting at the given byte offset"
	^self integerAt: byteOffset put: value size: 4 signed: true