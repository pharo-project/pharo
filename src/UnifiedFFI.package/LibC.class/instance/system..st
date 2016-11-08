misc
system: command
	^ self ffiCall: #(int system #(char * command))