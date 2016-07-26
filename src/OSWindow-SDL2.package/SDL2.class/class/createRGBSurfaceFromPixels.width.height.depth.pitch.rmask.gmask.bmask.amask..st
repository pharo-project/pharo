video
createRGBSurfaceFromPixels: pixels width: width height: height depth: depth pitch: pitch
	rmask: rmask gmask: gmask bmask: bmask amask: amask
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Surface* SDL_CreateRGBSurfaceFrom 
						( void* pixels , int width , int height , int depth , int pitch ,
						  Uint32 rmask , Uint32 gmask , Uint32 bmask , Uint32 amask ) )