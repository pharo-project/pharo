pixels handling
lockPixels: pixels pitch: pitch
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_LockTexture ( self , nil , void** pixels , int* pitch ) )