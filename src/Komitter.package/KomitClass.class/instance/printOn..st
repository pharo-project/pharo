printing
printOn: aStream

	super printOn: aStream.
	aStream << '[ '.
	self trackedClass printOn: aStream.
	aStream << ' ]'.