misc
loadLibrary: filename flag: flag
	^ self ffiCall: #(void *dlopen(const char *filename, int flag))