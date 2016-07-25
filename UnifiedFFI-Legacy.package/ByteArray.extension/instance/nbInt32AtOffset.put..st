*UnifiedFFI-Legacy
nbInt32AtOffset: zeroBasedOffset put: value
	"Store signed 32-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedLongAt: zeroBasedOffset + 1 put: value