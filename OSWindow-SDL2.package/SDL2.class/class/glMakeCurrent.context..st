opengl
glMakeCurrent: window context: context
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_GL_MakeCurrent ( SDL_Window window , SDL_GLContext context ) )