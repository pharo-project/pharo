cairo
primCreateRGBSurfaceForCairoWidth: width height: height
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Surface* SDL_CreateRGBSurface 
						( 0 , int width , int height , 32 ,
						  16r00FF0000 , 16r0000FF00 , 16r000000FF , 0 ) )