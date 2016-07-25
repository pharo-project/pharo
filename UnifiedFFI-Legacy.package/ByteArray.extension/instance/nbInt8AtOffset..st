*UnifiedFFI-Legacy
nbInt8AtOffset: zeroBasedOffset
	"Reads signed 8-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedByteAt: zeroBasedOffset + 1