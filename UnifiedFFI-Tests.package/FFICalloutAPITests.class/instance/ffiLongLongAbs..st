primitives atomic
ffiLongLongAbs: number
	^ self ffiCall: #( longlong llabs (longlong number) )