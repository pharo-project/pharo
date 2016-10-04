private
primCreateRenderer: driverIndex flags: flags
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Renderer SDL_CreateRenderer ( self , int driverIndex , Uint32 flags ) )