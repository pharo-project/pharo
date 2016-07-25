opengl
glSetAttribute: attr value: value
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_GL_SetAttribute(SDL_GLattr attr, int value) )