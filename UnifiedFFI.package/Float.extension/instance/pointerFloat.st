*UnifiedFFI
pointerFloat
	"sizeof(float)== 4"
	^ (ByteArray new: FFIFloat32 externalTypeSize)
		floatAt: 1 put: self;
		yourself