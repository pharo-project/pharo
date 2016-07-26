mouse
mouseState
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Uint32 SDL_GetMouseState ( nil , nil ) )