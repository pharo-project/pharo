*UnifiedFFI
asExternalTypeOn: generator
	^ FFIConst value: self type: (generator resolveExternalType: #int32)