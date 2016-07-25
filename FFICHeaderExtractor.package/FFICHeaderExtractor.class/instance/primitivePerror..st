primitives
primitivePerror: anErrorString

	^ self ffiCall: #( void perror(String anErrorString) )
