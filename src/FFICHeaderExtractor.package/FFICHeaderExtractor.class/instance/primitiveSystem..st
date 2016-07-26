primitives
primitiveSystem: commandString
	 ^ self ffiCall: #(int system(char* commandString) ) 
		