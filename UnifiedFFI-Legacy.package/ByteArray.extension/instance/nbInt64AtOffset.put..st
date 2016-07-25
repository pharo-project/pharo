*UnifiedFFI-Legacy
nbInt64AtOffset: zeroBasedOffset put: value
	"Store signed 64-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedLongAt: zeroBasedOffset + 1 put: value