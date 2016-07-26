accessing
title
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( char* SDL_GetWindowTitle ( self ) )