rendering
drawRect: rect
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_RenderDrawRect ( self , SDL_Rect* rect ) )