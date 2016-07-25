opengl
glGetProcAddress: procName
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void* SDL_GL_GetProcAddress ( String procName ) )