files - primitives
primitiveClose: aFileDescription 
	
	^ self ffiCall: #( int close(int aFileDescription) )
	