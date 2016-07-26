*UnifiedFFI-Legacy
nbUInt32AtOffset: zeroBasedOffset  put: value
	"Store unsigned 32-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self unsignedLongAt: zeroBasedOffset + 1 put: value