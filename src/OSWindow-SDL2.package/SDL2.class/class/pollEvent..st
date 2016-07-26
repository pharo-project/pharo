event
pollEvent: event
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_PollEvent ( SDL_Event event ) )