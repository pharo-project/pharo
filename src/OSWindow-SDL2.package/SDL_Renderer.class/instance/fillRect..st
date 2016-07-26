rendering
fillRect: rect
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_RenderFillRect ( self , SDL_Rect* rect ) )