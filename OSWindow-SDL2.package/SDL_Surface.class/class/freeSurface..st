video
freeSurface: aHandle
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_FreeSurface ( ulong aHandle ) )