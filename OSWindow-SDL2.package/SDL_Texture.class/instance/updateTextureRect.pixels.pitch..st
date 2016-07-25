initialize
updateTextureRect: rect pixels: pixels pitch: pitch
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_UpdateTexture ( self , SDL_Rect* rect , void* pixels , int pitch ) )