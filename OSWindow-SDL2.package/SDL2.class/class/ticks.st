timer
ticks
	"Gets the number of milliseconds since the SDL library was initialized"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Uint32 SDL_GetTicks ( ) )