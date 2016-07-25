*UnifiedFFI
pointer 
	^ (ByteArray new: FFIExternalType pointerSize)
		unsignedLongAt: 1 put: self;
		yourself