*UnifiedFFI-Legacy
nbInt8AtOffset: zeroBasedOffset put: value
	"Store signed 8-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order"
	^ self signedByteAt: zeroBasedOffset + 1 put: value