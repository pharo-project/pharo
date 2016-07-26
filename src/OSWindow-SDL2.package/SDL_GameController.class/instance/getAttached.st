accessing
getAttached
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_bool SDL_GameControllerGetAttached(self) )