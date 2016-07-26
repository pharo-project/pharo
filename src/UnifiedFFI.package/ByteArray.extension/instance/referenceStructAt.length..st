*UnifiedFFI
referenceStructAt: byteOffset length: length
	^ FFIExternalStructureReferenceHandle 
		handle: self 
		offset: byteOffset