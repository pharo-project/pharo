files - primitives
primitiveFileno: file

	^ self ffiCall: #( int fileno( OSSCFile file) )
