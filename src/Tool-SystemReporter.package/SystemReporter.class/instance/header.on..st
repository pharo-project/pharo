printing-report
header: aString on: aStream
	aStream withAttribute: TextEmphasis bold do: [	
		aStream nextPutAll: aString; cr.
		aString size timesRepeat: [aStream nextPut: $-].
		aStream cr]