primitives atomic
ffiIntAbs: number
	^ self ffiCall: #( int abs ( int number ) )