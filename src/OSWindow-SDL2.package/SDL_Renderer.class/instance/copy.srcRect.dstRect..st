rendering
copy: texture srcRect: srcRect dstRect: dstRect
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_RenderCopy ( self , SDL_Texture texture , SDL_Rect* srcRect , SDL_Rect* dstRect ) )