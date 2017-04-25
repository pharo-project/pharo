*FFI-Kernel
unsignedLongLongAt: byteOffset
	"Answer a 64-bit integer in Smalltalk order (little-endian)."
	^self integerAt: byteOffset size: 8 signed: false