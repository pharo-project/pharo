helpers
writeEncoded: bytes
	
	fileName ifNil:[^self].
	FileStream forceNewFileNamed: fileName do: [:file | file nextPutAll: bytes]