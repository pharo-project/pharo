*UnifiedFFI-Legacy
nbUInt16AtOffset: zeroBasedOffset
	"Reads unsigned 16-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order"
	^ self unsignedShortAt: zeroBasedOffset + 1