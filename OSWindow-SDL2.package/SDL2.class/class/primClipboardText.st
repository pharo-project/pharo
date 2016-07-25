clipboard
primClipboardText
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( char* SDL_GetClipboardText ( ) )