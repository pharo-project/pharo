NOT CURRENTLY USED
primitiveSysconf: aNumber
	
	^ self ffiCall: #( long sysconf(int aNumber) )

