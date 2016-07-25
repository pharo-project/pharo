*OSWindow-SDL2-OpenGL
newOpenGLRenderer
	^ renderer := OSSDL2GLRenderer new 
		window: self;
		createContext;
		yourself