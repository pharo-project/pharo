common
wasInit: flags
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_WasInit ( Uint32 flags ) )