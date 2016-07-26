joystick
joystickOpen: deviceIndex
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Joystick SDL_JoystickOpen ( int deviceIndex ) )