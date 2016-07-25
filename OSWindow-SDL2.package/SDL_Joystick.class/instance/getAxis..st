accessing
getAxis: axis
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Sint16 SDL_JoystickGetAxis(self , int axis) )