primitives
primStrcpy: aString
	^ self 
		ffiCall: #( char *strcpy ( self, const char *aString ) )
		module: LibC