rendering
primCreateTextureFormat: format access: access width: width height: height
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Texture SDL_CreateTexture ( self , Uint32 format , int access , int width , int height ) )