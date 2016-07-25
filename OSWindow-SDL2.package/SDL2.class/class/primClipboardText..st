clipboard
primClipboardText: text
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_SetClipboardText ( char* text ) )