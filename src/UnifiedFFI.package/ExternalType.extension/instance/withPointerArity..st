*UnifiedFFI
withPointerArity: aNumber
	self flag: #todo. "FFI does not understand arity>1 (for now), so I will ignore it and return 
	 the #referencedType in case I want a pointer... this needs to be fixed."
	^ aNumber > 0  
		ifTrue: [ self asPointerType  ]
		ifFalse: [ self ]