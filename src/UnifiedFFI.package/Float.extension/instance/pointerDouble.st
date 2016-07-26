*UnifiedFFI
pointerDouble
	"sizeof(double)== 8"
	^ (ByteArray new: FFIFloat64 externalTypeSize)
		doubleAt: 1 put: self;
		yourself