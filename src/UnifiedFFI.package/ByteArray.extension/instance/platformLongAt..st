*UnifiedFFI
platformLongAt: byteOffset
	^ self 
		integerAt: byteOffset 
		size: FFIArchitecture forCurrentArchitecture longTypeSize 
		signed: true