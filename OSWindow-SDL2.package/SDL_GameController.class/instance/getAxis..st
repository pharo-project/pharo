accessing
getAxis: axis
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Sint16 SDL_GameControllerGetAxis(self , int axis) )