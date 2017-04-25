misc
lastError
	^ self ffiCall: #(String dlerror(void))