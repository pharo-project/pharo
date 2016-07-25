accessing
fullscreen: flags
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_SetWindowFullscreen ( self , Uint32 flags ) )