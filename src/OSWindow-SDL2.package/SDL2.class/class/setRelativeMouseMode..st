mouse
setRelativeMouseMode: enabled
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_SetRelativeMouseMode ( bool enabled ) )