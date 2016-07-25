*UnifiedFFI-Legacy
nbUInt16AtOffset: zeroBasedOffset put: value
	"Store unsigned 16-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order"
	^ self unsignedShortAt: zeroBasedOffset + 1 put: value