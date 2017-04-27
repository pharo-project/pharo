*UnifiedFFI
signedByteAt: byteOffset put: value
	"Store a 8bit signed integer starting at the given byte offset"
	^self integerAt: byteOffset put: value size: 1 signed: true