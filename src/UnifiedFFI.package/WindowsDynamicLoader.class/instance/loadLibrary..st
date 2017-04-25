misc
loadLibrary: lpFileName
	^ self ffiCall: #(void *LoadLibrary(String lpFileName))