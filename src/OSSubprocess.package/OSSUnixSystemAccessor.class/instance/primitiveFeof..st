files - primitives
primitiveFeof: filePointer

	^ self ffiCall: #( int feof( OSSCFile filePointer) )
