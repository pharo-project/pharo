primitives pointer
primFromByteArray: src toExternalAddress: dest size: n
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode >
	^ self 
		ffiCall: #( void *memcpy(ExternalAddress dest, ByteArray src, size_t n) )
		module: LibC