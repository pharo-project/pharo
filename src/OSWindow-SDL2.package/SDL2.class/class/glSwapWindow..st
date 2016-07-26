opengl
glSwapWindow: window
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_GL_SwapWindow ( SDL_Window window ) )