class factory
ofType: aTypeName size: elements
	^ FFITypeArray newAnonymousSubclass 
		type: (FFIExternalType resolveType: aTypeName)	
		size: elements