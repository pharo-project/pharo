misc
createContext
	self onRenderThreadBlocking: [
		context := SDL2 glCreateContext: window handle.
	]
