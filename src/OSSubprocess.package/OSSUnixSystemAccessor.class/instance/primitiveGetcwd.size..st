cwd - primitives
primitiveGetcwd: buffer size: size
	
	^ self ffiCall: #( char * getcwd(char *buffer, int size) )
	