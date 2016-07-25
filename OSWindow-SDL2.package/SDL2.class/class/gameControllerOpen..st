game controller
gameControllerOpen: deviceIndex
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_GameController SDL_GameControllerOpen ( int deviceIndex ) )