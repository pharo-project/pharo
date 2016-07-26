joystick
numberOfJoysticks
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_NumJoysticks() )