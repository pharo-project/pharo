accessing
name
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( String SDL_GameControllerName(self) )