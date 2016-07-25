primitives
primObject: anObject strcpy: aString
	self
		ffiCall: #( char *strcpy ( FFICalloutObjectForTest anObject, const char *aString ) )
		module: LibC