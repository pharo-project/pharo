timer
delay: milliseconds
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_Delay ( Uint32 milliseconds ) )