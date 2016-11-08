misc
loadLibrary: path
	^ self ffiCall: #(void* dlopen(char* path, 0))