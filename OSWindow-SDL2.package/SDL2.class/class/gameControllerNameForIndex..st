game controller
gameControllerNameForIndex: deviceIndex
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_String SDL_GameControllerNameForIndex ( int deviceIndex ) )