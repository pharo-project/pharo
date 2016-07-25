NOT CURRENTLY USED
primitiveFdopen: aFileDescription mode: aMode
	
	^ self ffiCall: #( OSSCFile fdopen(int aFileDescription, String aMode) )
	