accessing
environmentStrings
	 ^ self ffiCall: #( void * GetEnvironmentStrings () ) module: #Kernel32