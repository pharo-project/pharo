*UnifiedFFI-Legacy
nbFloat32AtOffset: zeroBasedOffset put: value
	"Store 32-bit float at ZERO-based index."
	^ self floatAt: zeroBasedOffset + 1 put: value