misc
error
	^ self ffiCall: #(String dlerror(void))