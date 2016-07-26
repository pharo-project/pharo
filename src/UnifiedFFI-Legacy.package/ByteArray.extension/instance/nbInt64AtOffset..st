*UnifiedFFI-Legacy
nbInt64AtOffset: zeroBasedOffset
	"Reads signed 64-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedLongAt: zeroBasedOffset + 1