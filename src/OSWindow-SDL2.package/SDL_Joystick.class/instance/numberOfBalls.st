accessing
numberOfBalls
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_JoystickNumBalls(self) )