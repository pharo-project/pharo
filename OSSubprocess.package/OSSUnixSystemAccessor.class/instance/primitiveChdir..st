cwd - primitives
primitiveChdir: aDirString
	
	^ self ffiCall: #( int chdir(String aDirString) )
	