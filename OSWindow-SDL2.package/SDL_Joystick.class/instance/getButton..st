accessing
getButton: button
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Sint16 SDL_JoystickGetButton(self , int button) )