mouse
modState
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Keymod SDL_GetModState ( ) )