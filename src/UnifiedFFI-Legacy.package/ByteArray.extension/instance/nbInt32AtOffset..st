*UnifiedFFI-Legacy
nbInt32AtOffset: zeroBasedOffset
	"Reads signed 32-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedLongAt: zeroBasedOffset + 1