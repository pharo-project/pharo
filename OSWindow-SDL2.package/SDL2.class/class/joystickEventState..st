joystick
joystickEventState: state
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Joystick SDL_JoystickEventState ( int state ) )