primitives pointer
primStr: aString cat: otherString  
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode >
	^ self 
		ffiCall: #( char *strcat ( char *aString, char *otherString ) )
		module: LibC
