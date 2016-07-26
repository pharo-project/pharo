accessing
setPositionX: x y: y
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_SetWindowPosition ( self , int x , int y ) )