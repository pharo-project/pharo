error handing
getErrorMessage
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( String SDL_GetError ( void ) )