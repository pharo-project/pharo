testing
isAvailable
	self flag: #todo. "Replace this for a check of FFI available"
	self environment at: #FFICalloutAPI ifAbsent: [ ^ false ].
	^ true