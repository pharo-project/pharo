*UnifiedFFI-Legacy
nbUInt64AtOffset: zeroBasedOffset put: value
	"Store unsigned 64-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order"
	^ self unsignedLongLongAt: zeroBasedOffset + 1 put: value