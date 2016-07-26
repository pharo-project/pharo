surface
getFlags
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Uint32 SDL_GetWindowFlags ( self ) )