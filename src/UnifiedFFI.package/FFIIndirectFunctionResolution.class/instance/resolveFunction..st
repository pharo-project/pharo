resolving
resolveFunction: anExternalFunction
	^ (ExternalAddress 
		loadSymbol: anExternalFunction name
		module: anExternalFunction module)
		pointerAt: 1.