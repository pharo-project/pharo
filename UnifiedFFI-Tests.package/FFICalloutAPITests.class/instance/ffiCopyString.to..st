primitives atomic
ffiCopyString: aString to: dest
	^ self ffiCall: #( String strcpy ( String dest, String aString ) )