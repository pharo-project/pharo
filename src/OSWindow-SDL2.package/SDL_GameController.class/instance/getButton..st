accessing
getButton: button
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Uint8 SDL_GameControllerGetButton(self , int button) )