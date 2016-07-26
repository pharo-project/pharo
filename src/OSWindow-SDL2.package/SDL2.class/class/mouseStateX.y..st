mouse
mouseStateX: x y: y
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Uint32 SDL_GetMouseState ( int* x , int* y ) )