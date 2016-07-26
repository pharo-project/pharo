surface
updateWindowSurface
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_UpdateWindowSurface( self ) )