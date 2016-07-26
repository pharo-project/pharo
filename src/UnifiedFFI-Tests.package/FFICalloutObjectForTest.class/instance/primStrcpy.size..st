primitives
primStrcpy: aString size: n
	^ self 
		ffiCall: #( char *strncpy ( self, const char *aString, size_t n ) )
		module: LibC