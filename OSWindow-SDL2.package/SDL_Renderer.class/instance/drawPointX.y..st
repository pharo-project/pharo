rendering
drawPointX: x y: y
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_RenderDrawPoint ( self , int x , int y ) )