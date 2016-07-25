cursor
showCursor: toggle
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_ShowCursor ( int toggle ) )