freeing resources
freeCursor
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_FreeCursor ( self ) )