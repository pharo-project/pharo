accessing
windowID
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Uint32 SDL_GetWindowID ( self ) )