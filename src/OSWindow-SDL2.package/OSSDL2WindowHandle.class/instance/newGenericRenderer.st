instance creation
newGenericRenderer
	^ renderer := OSSDL2GenericRenderer new 
		window: self;
		createSDL2Renderer;
		yourself