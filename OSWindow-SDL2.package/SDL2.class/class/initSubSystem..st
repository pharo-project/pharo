common
initSubSystem: flags
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_InitSubSystem ( Uint32 flags ) )