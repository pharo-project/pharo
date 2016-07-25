primitives pointer
primMemMoveFrom: src to: dest size: n
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode >
	^ self 
		ffiCall: #( void *memcpy(void *dest, const void *src, size_t n) )
		module: LibC