rendering
noClipRect
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_RenderSetClipRect ( self , nil ) )