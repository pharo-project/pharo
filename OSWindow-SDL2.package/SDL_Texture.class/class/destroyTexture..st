finalization
destroyTexture: aHandle
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_DestroyTexture ( ulong aHandle ) )