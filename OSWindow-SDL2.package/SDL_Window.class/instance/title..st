accessing
title: title
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_SetWindowTitle ( self , String title ) )