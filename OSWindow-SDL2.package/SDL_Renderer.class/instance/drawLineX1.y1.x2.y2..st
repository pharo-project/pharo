rendering
drawLineX1: x1 y1: y1 x2: x2 y2: y2
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_RenderDrawLine ( self , int x1 , int y1 , int x2 , int y2 ) )