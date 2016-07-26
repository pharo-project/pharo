window management
getWMInfo: infos
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( bool SDL_GetWindowWMInfo( self, SDL_SysWMinfo* infos) )