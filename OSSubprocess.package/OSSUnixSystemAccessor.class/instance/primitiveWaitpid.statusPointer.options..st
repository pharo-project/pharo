waiting - primitives
primitiveWaitpid: aProcessId statusPointer: statusPointer options: optionBits
	
	^ self ffiCall: #( int waitpid(int aProcessId, void* statusPointer, int optionBits) )