video
createWindow: title x: x y: y width: w height: h flags: flags
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Window SDL_CreateWindow ( String title, int x, int y, int w, int h, Uint32 flags ) )