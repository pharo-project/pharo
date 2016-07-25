printing
printOn: aStream

	super printOn: aStream.
	package name ifNotNil: [ aStream nextPutAll: '(' , package name asString, ')'].