accessing
getPositionX: x y: y
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_GetWindowPosition ( self , int* x , int* y ) )