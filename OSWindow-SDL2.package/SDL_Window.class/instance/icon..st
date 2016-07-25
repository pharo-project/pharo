accessing
icon: surface
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_SetWindowIcon ( self , SDL_Surface* surface ) )