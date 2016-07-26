NOT CURRENTLY USED
primitiveGetenv: variableName
	
	^ self ffiCall: #( char * getenv(char* variableName) )