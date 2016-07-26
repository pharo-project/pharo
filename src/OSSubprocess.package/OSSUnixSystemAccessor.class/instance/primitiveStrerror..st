errors - primitives
primitiveStrerror: errorNumber
	
	^ self ffiCall: #( String strerror( int errorNumber) )
