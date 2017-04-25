*UnifiedFFI
platformLongAt: byteOffset put: value
	self 
		integerAt: byteOffset
		put: value
		size: FFIArchitecture forCurrentArchitecture longTypeSize
		signed: true