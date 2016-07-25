testing
canReadFileNamed: aString
	| reader |
	reader := MCVersionReader readerClassForFileNamed: aString.
	^ reader notNil