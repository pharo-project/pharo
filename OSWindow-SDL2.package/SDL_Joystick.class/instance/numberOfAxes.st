accessing
numberOfAxes
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_JoystickNumAxes(self) )