opengl
glCreateContext: window
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_GLContext SDL_GL_CreateContext ( SDL_Window window ) )