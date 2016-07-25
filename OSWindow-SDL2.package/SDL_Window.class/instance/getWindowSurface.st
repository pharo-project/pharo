surface
getWindowSurface
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Surface* SDL_GetWindowSurface( self ) )