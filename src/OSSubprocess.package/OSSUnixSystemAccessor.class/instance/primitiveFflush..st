files - primitives
primitiveFflush: file

	^ self ffiCall: #( int fflush( OSSCFile file) )
