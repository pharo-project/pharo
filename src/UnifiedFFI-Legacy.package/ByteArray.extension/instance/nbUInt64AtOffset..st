*UnifiedFFI-Legacy
nbUInt64AtOffset: zeroBasedOffset
	"Reads unsigned 64-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self unsignedLongLongAt: zeroBasedOffset + 1