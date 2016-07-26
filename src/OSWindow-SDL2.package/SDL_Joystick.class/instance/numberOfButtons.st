accessing
numberOfButtons
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_JoystickNumButtons(self) )