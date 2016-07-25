accessing
setSizeW: w h: h
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_SetWindowSize ( self , int w , int h ) )