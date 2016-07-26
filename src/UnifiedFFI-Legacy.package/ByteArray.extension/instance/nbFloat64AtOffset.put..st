*UnifiedFFI-Legacy
nbFloat64AtOffset: zeroBasedOffset put: value
	"Store 64-bit float at ZERO-based index."
	^ self doubleAt: zeroBasedOffset + 1 put: value