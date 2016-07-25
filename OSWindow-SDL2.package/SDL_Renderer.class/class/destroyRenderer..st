finalization
destroyRenderer: aHandle
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_DestroyRenderer ( ulong aHandle ) )