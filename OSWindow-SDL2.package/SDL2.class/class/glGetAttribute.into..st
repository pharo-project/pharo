opengl
glGetAttribute: attr into: value
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_GL_GetAttribute(SDL_GLattr attr, int *value) )