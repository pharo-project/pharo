rendering
drawColorR: r g: g b: b a: a
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_SetRenderDrawColor ( self , Uint8 r , Uint8 g , Uint8 b , Uint8 a ) )