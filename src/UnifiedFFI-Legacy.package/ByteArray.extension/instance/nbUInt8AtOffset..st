*UnifiedFFI-Legacy
nbUInt8AtOffset: zeroBasedOffset
	"Reads unsigned 8-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self unsignedByteAt: zeroBasedOffset + 1