rendering
copy: texture
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_RenderCopy ( self , SDL_Texture texture , nil, nil ) )