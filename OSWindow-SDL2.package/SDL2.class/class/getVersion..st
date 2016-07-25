common
getVersion: ver
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_GetVersion(SDL_Version* ver) )