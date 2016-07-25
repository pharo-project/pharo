instance creation
for: aWindowHandle

	^ self new 
		window: aWindowHandle;
		checkSession;
		createRenderer;
		createTexture;
		createSDLSurface;
		createAthensSurface;
		yourself.