common
quitSubSystem: flags
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_QuitSubSystem ( Uint32 flags ) )