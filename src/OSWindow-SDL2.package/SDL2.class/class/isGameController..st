game controller
isGameController: deviceIndex
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( bool SDL_IsGameController ( int deviceIndex ) )