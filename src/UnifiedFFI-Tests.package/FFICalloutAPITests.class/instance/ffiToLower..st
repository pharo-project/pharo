primitives atomic
ffiToLower: c
	^ self ffiCall: #( char tolower (char c) )