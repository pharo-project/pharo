cursor
createCursor: data mask: mask w: w h: h hotX: hotX hotY: hotY
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Cursor SDL_CreateCursor ( Uint8* data, Uint8* mask, int w, int h, int hotX, int hotY ) )