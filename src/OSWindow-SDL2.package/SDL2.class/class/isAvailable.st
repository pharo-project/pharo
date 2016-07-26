common
isAvailable
	^ [ (ExternalAddress loadSymbol: 'SDL_Init' from: SDL2 moduleName) notNil ] 	
		ifError: [ false ]