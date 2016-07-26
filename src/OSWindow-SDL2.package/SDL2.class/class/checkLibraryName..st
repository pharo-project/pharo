native
checkLibraryName: aName
	^ (ExternalAddress loadSymbol: 'SDL_Init' from: aName) notNil