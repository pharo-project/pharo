accessing
getSizeW: w h: h
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_GetWindowSize ( self , int* w , int* h ) )