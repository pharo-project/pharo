printing
printOn: aStream

	super printOn: aStream.
	aStream << '[ '.
	self package name printOn: aStream.
	aStream << ' ]'.