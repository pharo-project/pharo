video
freeSurface: surface
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_FreeSurface ( SDL_Surface* surface ) )