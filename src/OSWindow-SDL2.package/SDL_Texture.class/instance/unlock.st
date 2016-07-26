pixels handling
unlock
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_UnlockTexture ( self ) )