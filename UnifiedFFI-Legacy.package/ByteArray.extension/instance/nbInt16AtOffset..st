*UnifiedFFI-Legacy
nbInt16AtOffset: zeroBasedOffset
	"Reads signed 16-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedShortAt: zeroBasedOffset + 1