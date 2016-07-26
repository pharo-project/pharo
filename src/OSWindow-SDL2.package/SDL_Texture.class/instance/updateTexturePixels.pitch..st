initialize
updateTexturePixels: pixels pitch: pitch
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_UpdateTexture ( self , nil , void* pixels , int pitch ) )