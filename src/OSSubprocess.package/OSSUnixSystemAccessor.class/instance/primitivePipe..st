NOT CURRENTLY USED
primitivePipe: pipePointer
	
	^ self ffiCall: #( int pipe(void* pipePointer) )