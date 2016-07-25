window management
toggleBorder: bordered
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_SetWindowBordered( self, bool bordered ) )