printing
printOn: aStream

	super printOn: aStream.
	aStream << '[ '.
	self content package name printOn: aStream.
	aStream << ' ]'.