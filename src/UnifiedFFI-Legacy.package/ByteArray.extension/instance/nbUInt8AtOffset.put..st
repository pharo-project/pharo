*UnifiedFFI-Legacy
nbUInt8AtOffset: zeroBasedOffset put: value
	"Store unsigned 8-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self unsignedByteAt: zeroBasedOffset + 1 put: value