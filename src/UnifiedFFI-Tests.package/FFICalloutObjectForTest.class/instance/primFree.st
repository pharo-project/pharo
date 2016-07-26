primitives
primFree
	^ self 
		ffiCall: #( void free ( self ) )
		module: LibC