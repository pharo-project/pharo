*UnifiedFFI-Legacy
nbUInt32AtOffset: zeroBasedOffset
	"Reads unsigned 32-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self unsignedLongAt: zeroBasedOffset + 1