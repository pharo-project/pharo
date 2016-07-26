*UnifiedFFI-Legacy
nbInt16AtOffset: zeroBasedOffset put: value
	"Store signed 16-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedShortAt: zeroBasedOffset + 1 put: value