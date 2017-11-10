Extension { #name : #OSSDL2WindowHandle }

{ #category : #'*OSWindow-SDL2-OpenGL' }
OSSDL2WindowHandle >> newOpenGLRenderer [
	^ renderer := OSSDL2GLRenderer new 
		window: self;
		createContext;
		yourself
]
